;;; magit-gitlab.el --- Magit plugin for manipulating GitLab merge requests  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Arvid Jakobsson

;; Author: Arvid Jakobsson <arvid.jakobsson@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (magit "3.3.0") (ghub "3.6.0") (transient "0.6.0"))
;; Created: 15 Jun 2024

;; Keyword: git tools vc
;; URL: https://gitlab.com/arvidnl/magit-gitlab.el

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; “Software”), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; magit-gitlab.el provides a transient for acting upon GitLab merge
;; requests: editing title, description, labels, assignees, reviewers
;; etc.

;;; Code:

(require 'magit)
(require 'glab)

;; Customization

(defgroup magit-gitlab nil
  "Manipulating GitLab merge requests from Emacs."
  :prefix "magit-glab-"
  :group 'convenience)

(defcustom mg-favorite-users nil
  "A list of GitLab favorite users for easy access.

Should be a list of values, where each value is on the list
PREFIX DESCRIPTION GITLAB_USERNAME.

For the prefix, use lower-case letters.  Each prefix should be unique.

Example:
  \\='((\"i\" \"Myself\" \"@arvidnl\")
    (\"j d\" \"John Doe\" \"@...\"))"
  :group 'magit-gitlab
  :type
  '(repeat
    (list
     (string :tag "Prefix")
     (string :tag "Full name for display")
     (string :tag "GitLab username (include @)"))))

;; Internals

;;; Caches

(defvar mg--known-milestones (make-hash-table :test 'equal)
  "Hash table for storing seen milestones.

Used to provide candidates in completing-reads.")

;;; Encoding

(defun mg-url-encode-project-id (project-id)
  "Encode a GitLab PROJECT-ID as a string.

A GitLab PROJECT-ID can either be a string on the form
NAMESPACE/PROJECT.  In this case, the return value is the
URL-encoding of this string.  The PROJECT-ID can be an integer, in
which case the return value is the decimal representation of the
integer as a string."
  (if (numberp project-id)
      (number-to-string project-id)
    (url-hexify-string project-id)))

(defun mg--url-mr (project-id mr-iid)
  "Path to GitLab API's `Get single MR' endpoint for PROJECT-ID and MR-IID.

Return path to the GitLab API's `Get single MR' endpoint for a
given PROJECT-ID (which can be a string on the form
NAMESPACE/PROJECT or integral) and an integer MR-IID.

For more information, see URL
`https://docs.gitlab.com/ee/api/merge_requests.html#get-single-mr'."
  (concat
   "/projects/"
   (mg-url-encode-project-id project-id)
   "/merge_requests/"
   (number-to-string mr-iid)))

(defun mg--url-project (project-id)
  "Path to GitLab API's `Get single project' endpoint for PROJECT-ID.

Return path to the GitLab API's `Get single project' endpoint for a
given PROJECT-ID (which can be a string on the form
NAMESPACE/PROJECT or integral).

For more information, see URL
`https://docs.gitlab.com/ee/api/projects.html#get-single-project'."
  (concat "/projects/" (mg-url-encode-project-id project-id)))

(defvar mg--GET-cache-file "/tmp/.magit-glab-cache.el"
  "Path to magit-gitlab's cache.")

(defvar mg--GET-cache (make-hash-table :test 'equal)
  "Hash table for storing memoized GET results.")

(cl-defun mg--get (resource params &key no-cache callback errorback)
  "Make a request for RESOURCE with caching and return the response body.

This function is as `ghub-request' with METHOD set to GET, and
the arguments RESOURCE, PARAMS, CALLBACK and ERRORBACK has the
same meaning.  However, if the request is successful, the
response is cached in `mg--GET-cache'.  Subsequent calls
to `mg--get', with exactly matching REQUEST and PARAMS,
return the same value unless NO-CACHE is non-nil.  When a cached
value is returned, all argument except the first passed to
CALLBACK are nil."
  (if no-cache
      (ghub-request
       "GET"
       resource
       params
       :auth 'magit-gitlab
       :forge 'gitlab
       :callback callback
       :errorback errorback)
    (if-let (cached-value
             (gethash (list resource params) mg--GET-cache))
      ;; Value found in cache
      (if callback
          (funcall callback cached-value nil nil nil)
        cached-value)
      ;; No value in cache
      (if (or callback errorback)
          ;; If asynchronous
          (ghub-request
           "GET" resource params
           :auth 'magit-gitlab
           :forge 'gitlab
           :callback
           (lambda (resp header status req)
             (puthash (list resource params) resp mg--GET-cache)
             (funcall callback resp header status req))
           :errorback errorback)
        ;; If synchronous
        (if-let (value
                 (ghub-request
                  "GET"
                  resource
                  params
                  :auth 'magit-gitlab
                  :forge 'gitlab))
          (puthash (list resource params) value mg--GET-cache))))))

(cl-defun mg--get1 (url params &key no-cache callback errorback)
  "Like `mg--get', but apply car to the result.

See `magit-gitlab--get' for details on URL, PARAMS, NO-CACHE, CALLBACK
and ERRORBACK."
  (let ((resp
         (mg--get
          url params
          :callback
          (when callback
            (lambda (resp) (funcall callback (car resp))))
          :errorback errorback
          :no-cache no-cache)))
    (when (not (or callback errorback))
      (car resp))))

(cl-defun mg--get-user (username &key no-cache callback errorback)
  "Get the user corresponding to USERNAME.

See `magit-gitlab--get` for details on NO-CACHE, CALLBACK and ERRORBACK."
  (mg--get1
   "/users"
   `((username . ,username))
   :no-cache no-cache
   :callback callback
   :errorback errorback))

(cl-defun mg--get-milestone-of-iid
    (project-id milestone-iid &key no-cache callback errorback)
  "Get the milestone corresponding to MILESTONE-IID in PROJECT-ID.

See `magit-gitlab--get' for details on NO-CACHE, CALLBACK and ERRORBACK."
  (mg--get1
   (format "/projects/%s/milestones"
           (mg-url-encode-project-id project-id))
   `((iids . ,milestone-iid))
   :no-cache no-cache
   :callback callback
   :errorback errorback))

(cl-defun mg--get-mr
    (project-id mr-iid &key no-cache callback errorback)
  "Get the MR corresponding to MR-IID in PROJECT-ID.

See `magit-gitlab--get' for details on NO-CACHE, CALLBACK and ERRORBACK."
  (mg--get
   (mg--url-mr project-id mr-iid)
   nil
   :callback callback
   :errorback errorback
   :no-cache no-cache))


(cl-defun mg--get-mr-of-source-branch
    (project-id source-branch &key no-cache callback errorback)
  "Get the MR corresponding to SOURCE-BRANCH in PROJECT-ID.

See `magit-gitlab--get' for details on NO-CACHE, CALLBACK and ERRORBACK."
  (let ((observe-milestone
         (lambda (mr)
           (when-let (milestone
                      (alist-get 'milestone mr))
             (puthash
              (alist-get 'id milestone)
              milestone
              mg--known-milestones)))))
    (let ((resp
           (mg--get1
            (concat
             "/projects/"
             (mg-url-encode-project-id project-id)
             "/merge_requests")
            `((source_branch . ,source-branch))
            :callback
            (when callback
              (lambda (resp header status req)
                (funcall observe-milestone resp)
                (funcall callback resp header status req)))
            :errorback errorback
            :no-cache no-cache)))
      (when (not (or callback errorback))
        (funcall observe-milestone resp)
        resp))))

(cl-defun mg--get-project
    (project-id &key no-cache callback errorback)
  "Get project corresponding  PROJECT-ID.

See `magit-gitlab--get' for details on NO-CACHE, CALLBACK and ERRORBACK."
  (mg--get
   (mg--url-project project-id)
   nil
   :callback callback
   :errorback errorback
   :no-cache no-cache))

(defconst mg--mr-properties
  '(add_labels
    allow_collaboration
    allow_maintainer_to_push
    assignee_id
    assignee_ids
    description
    discussion_locked
    labels
    milestone_id
    remove_labels
    remove_source_branch
    reviewer_ids
    squash
    state_event
    target_branch
    title))

(defun mg--show-mr-property (property)
  (pcase property
    ('add_labels "add labels") ;; todo
    ('allow_collaboration "allow collaboration")
    ('allow_maintainer_to_push "allow maintainer to push")
    ('assignee_id "assignee")
    ('assignee_ids "assignees")
    ('description "description")
    ('discussion_locked "discussion locked")
    ('labels "labels")
    ('milestone_id "milestone")
    ('remove_labels "remove labels")
    ('remove_source_branch "remove source branch")
    ('reviewer_ids "reviewers")
    ('squash "squash")
    ('state_event "state")
    ('target_branch "target branch")
    ('title "title")
    (_
     (error
      "Property %s is not one of: %s"
      property
      (mapconcat #'symbol-name mg--mr-properties ", ")))))

(defun mg--show-mr (mr)
  (format "%s!%d"
          (alist-get
           'path_with_namespace
           (mg--get-project (alist-get 'project_id mr)))
          (alist-get 'iid mr)))

(defun mg--format (mr string &rest objects)
  (concat
   "["
   (propertize (mg--show-mr mr) 'face 'magit-branch-local)
   "] "
   (apply 'format string objects)))

(defun mg--message (mr string &rest objects)
  (message (apply 'mg--format mr string objects)))

(cl-defun mg--mr-set-prop-async
    (mr
     property
     value
     &key
     callback
     errorback
     message-progress
     message-success
     message-error
     show-value)
  (unless (memq property mg--mr-properties)
    (error
     "Unsupported property: %s. Accepted properties are: %s"
     property
     (mapconcat #'symbol-name mg--mr-properties ", ")))
  (let* ((value-pp
          (if show-value
              (funcall show-value value)
            value))
         (message-prog
          (or message-progress
              (mg--format mr "Setting %s%s"
                          (mg--show-mr-property property)
                          (if value-pp
                              (format " to: '%s'" value-pp)
                            ""))))
         (message-success
          (or message-success (format "%s... Done!" message-prog)))
         (message-error
          (or message-error
              (mg--format mr "An error occurred when setting %s:"
                          (mg--show-mr-property property)))))
    (message "%s..." message-prog)
    (ghub-request
     "PUT"
     (mg--url-mr (alist-get 'project_id mr) (alist-get 'iid mr))
     `((,property . ,value))
     :auth 'magit-gitlab
     :forge 'gitlab
     :callback
     (or callback
         (lambda (_resp _header _status _req)
           (message "%s" message-success)))
     :errorback
     (or errorback
         (lambda (err _header _status _req)
           (message "%s: %s" message-error err))))))

(defun mg--project-of-remote (remote-url)
  "Extract NAMESPACE/PROJECT from GitLab REMOTE-URL.

URL is a git repository URL in either of these forms:
- git@gitlab.com:NAMESPACE/PROJECT.git
- https://gitlab.com/NAMESPACE/PROJECT.git

Returns the 'NAMESPACE/PROJECT' part of the URL."
  ;; git@gitlab.com:tezos/tezos.git
  ;; https://gitlab.com/tezos/tezos.git
  (if (string-match
       "\\(git@gitlab\.com:\\|https://gitlab\.com/\\)\\([^/]+/[^/.]+\\)\\.git"
       remote-url)
      (match-string 2 remote-url)
    (error
     "Remote URL '%s' does not match expected format for a GitLab remote"
     remote-url)))

(defun mg--infer-project-id (branch)
  (if-let (remote
           (or (let ((branch_remote
                      (magit-get (format "branch.%s.remote" branch))))
                 (unless (string= branch_remote ".")
                   branch_remote))
               (let ((branch-push-remote
                      (magit-get
                       (format "branch.%s.pushRemote" branch))))
                 (unless (string= branch-push-remote ".")
                   branch-push-remote))
               (magit-get-current-remote)
               (magit-get "remote.pushDefault")))
    (if-let ((remote-url (magit-get (format "remote.%s.url" remote))))
      (mg--project-of-remote remote-url)
      (error
       "The remote '%s' has no url (remote.%s.url is not set)"
       remote
       remote))
    (error
     "Cannot infer GitLab project: no remote set for this branch, nor is remote.pushDefault set")))

(defvar-local mg--mr nil
  "Merge request under edit in MR description buffers.")

;; TODO: if saving fails, user should not lose their description.
(cl-defun mg-mr-save-description-buffer (&key callback errorback)
  "Save the current description buffer, updating the MR.

Calls CALLBACK if successful, ERRORBACK if not."
  (interactive)
  (mg--mr-set-prop-async
   mg--mr 'description (buffer-string)
   :show-value (lambda (_) nil)
   :callback
   (lambda (resp header status req)
     (set-buffer-modified-p nil)
     (mg--message mg--mr "Setting description... Done!")
     (when callback
       (funcall callback resp header status req)))
   :errorback errorback))

(defun mg-mr-save-and-close-description-buffer ()
  "Save and close the current description buffer, updating the MR."
  (interactive)
  (mg-mr-save-description-buffer
   :callback (lambda (_ _ _ _) (magit-kill-this-buffer))))

(defun mg-mr-cancel-description-buffer ()
  "Cancel the current description update buffer, does not update the MR."
  (interactive)
  (mg--message mg--mr "Description edit cancelled")
  (magit-kill-this-buffer))

(defun mg--mr-create-description-buffer (mr)
  "Create description buffer for MR."
  (let* ((base-name
          (format "Edit description of %s" (mg--show-mr mr)))
         (buffer-name base-name)
         (index 1))
    (while (get-buffer buffer-name)
      (setq buffer-name (format "%s<%d>" base-name index))
      (setq index (1+ index)))
    ;; Create and switch to the buffer
    (switch-to-buffer (get-buffer-create buffer-name))
    ;; Insert the description, if set.
    (when-let (description
               (alist-get 'description mr))
      (insert description))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (when (fboundp 'markdown-mode)
      (markdown-mode))
    ;; Set up local keybindings for this buffer
    (use-local-map (copy-keymap (current-local-map)))
    (setq mg--mr mr)
    (keymap-local-set
     "C-c C-c" #'mg-mr-save-and-close-description-buffer)
    (keymap-local-set "C-c C-s" #'mg-mr-save-description-buffer)
    (keymap-local-set "C-x C-s" #'mg-mr-save-description-buffer)
    (keymap-local-set "C-c C-k" #'mg-mr-cancel-description-buffer)
    ;; Return the newly created buffer
    buffer-name))

(defun mg--strip-remote-prefix (branch-name)
  "Strip the remote prefix from BRANCH-NAME if present."
  (let ((components (split-string branch-name "/")))
    (if (> (length components) 1)
        (mapconcat 'identity (cdr components) "/") ; Rejoin the rest if more than one slash exists
      branch-name))) ; Return the original if no slash found

;; By default, get current branch or branch-at-point. If prefix is
;; given or if both those values are nil, then read a value instead.
(defun mg--read-branch ()
  (let ((branch
         (or (magit-branch-at-point) (magit-get-current-branch))))
    (if (or current-prefix-arg (not branch))
        (read-string "Branch name: ")
      (mg--strip-remote-prefix branch))))

(cl-defun mg--read-mr (&key cache)
  "Read an MR.

Tries to deduce the MR the user intends

First, by trying to infer the GitLab project id associated with
the current branch (if any), by:

- investigating the remote of the current git branch;
- finding the git remote of that branch;
- matching it against known GitLab remotes.

If a GitLab project was detected, this function calls the GitLab
API to find out whether an MR has been created with a source
branch matching the current.  If CACHE is t, and a cached MR
might be returned."
  ;; TODO: if can't deduce branch -> ask for full mr ref
  ;; TODO: if can't deduce project -> ask for full mr ref
  ;; TODO: if can't deduce mr -> ask for full mr ref
  (let* ((branch (mg--read-branch))
         (project-id (mg--infer-project-id branch))
         (mr
          (mg--get-mr-of-source-branch
           project-id
           branch
           :no-cache (not cache))))
    (or mr
        (error
         "Couldn't find MR for branch '%s' in project '%s'"
         branch
         project-id))))

(defun mg--read-mr-scope ()
  "Get an MR from the scope if present, else call `magit-gitlab--read-mr'."
  (or (when transient-current-prefix
        (oref transient-current-prefix scope))
      (mg--read-mr)))

;;;###autoload (autoload 'magit-gitlab-mr-edit-description "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-edit-description
 (mr)
 "Edit the description of MR."
 (interactive (list (mg--read-mr-scope)))
 (mg--mr-create-description-buffer mr))

(transient-define-suffix
 mg-mr-edit-title (mr new-title) "Set the title of MR to NEW-TITLE."
 (interactive (let* ((mr (mg--read-mr-scope))
                     (new-title
                      (read-string (mg--format mr "New title: ")
                                   (alist-get 'title mr))))
                (list mr new-title)))
 (mg--mr-set-prop-async mr 'title new-title))

(transient-define-suffix
 mg-mr-edit-labels
 (mr new-labels)
 "Set the labels of MR to NEW-LABELS."
 (interactive (let* ((mr (mg--read-mr-scope))
                     (new-labels
                      (read-string (mg--format mr "New labels: ")
                                   (string-join (alist-get 'labels mr)
                                                ", "))))
                (list mr new-labels)))
 (mg--mr-set-prop-async mr 'labels new-labels))

;;;###autoload (autoload 'magit-gitlab-mr-edit-milestone "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-edit-milestone (mr new-milestone-iid)
 "Set the milestone of MR to NEW-MILESTONE-IID.

If NEW-MILESTONE-IID is 0, then the milestone is removed.
NEW-MILESTONE-IID is assumed to be in the same project as MR."
 ;; TODO: allow to read milestones like project/namespace%milestone
 ;; TODO: provide candidates
 ;; TODO: more tests
 ;; TODO: better progress messages when removing milestone
 (interactive (let* ((mr (mg--read-mr-scope))
                     (old-milestone
                      (alist-get 'iid (alist-get 'milestone mr)))
                     (new-milestone-iid
                      (string-to-number
                       (string-remove-prefix
                        "%"
                        (read-string (mg--format
                                      mr "New milestone id: ")
                                     (when old-milestone
                                       (number-to-string
                                        old-milestone)))))))
                (list mr new-milestone-iid)))
 (let ((new-milestone-id
        ;; Set milestone to 0 to un-assign
        (if (> new-milestone-iid 0)
            (alist-get
             'id
             (mg--get-milestone-of-iid
              (alist-get 'project_id mr) new-milestone-iid))
          new-milestone-iid)))
   (message "new-milestone-iid: %s, new-milestone-id: %s"
            new-milestone-iid
            new-milestone-id)
   (mg--mr-set-prop-async mr 'milestone_id new-milestone-id)))

;;;###autoload (autoload 'magit-gitlab-mr-edit-target-branch "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-edit-target-branch
 (mr target-branch)
 "Set the target branch of the MR associated to BRANCH to TARGET-BRANCH"
 (interactive (list
               (mg--read-mr-scope)
               (magit-read-other-branch "New target branch")))
 (mg--mr-set-prop-async mr 'target_branch target-branch))

;;;###autoload (autoload 'magit-gitlab-mr-toggle-draft "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-toggle-draft
 (mr)
 "Toggle the draft status of the MR associate to BRANCH"
 (interactive (list (mg--read-mr-scope)))
 (let* ((title (alist-get 'title mr))
        (is-draft (string-match "^\\(Draft: \\)+\\(.*\\)$" title))
        (new-title
         (if is-draft
             (let ((title-no-draft (match-string 2 title)))
               title-no-draft)
           (concat "Draft: " title))))
   (mg--mr-set-prop-async
    mr 'title new-title
    :message-progress
    (mg--format mr "%s as draft"
                (if is-draft
                    "Unmarking"
                  "Marking")))))

(defun mg--decode-assignees (assignee-objs)
  "From ASSIGNEE-OBJS to list of usernames (strings)."
  (mapcar
   (lambda (assignee-obj) (alist-get 'username assignee-obj))
   assignee-objs))

(defun mg--to-user-id (id-or-username)
  "From ID-OR-USERNAME to numerical user ids."
  (if (numberp id-or-username)
      id-or-username
    (if-let* ((assignee-username
               (string-remove-prefix "@" id-or-username)))
      (alist-get 'id (mg--get-user assignee-username))
      (error
       "Could not find id associated to username '%s' -- do they exist?"
       assignee-username))))

(defun mg--format-user-as-candidate (user)
  "Return USER as a cons (NAME (@USERNAME) . ID)."
  (let ((username (alist-get 'username user))
        (name (alist-get 'name user))
        (id (alist-get 'id user)))
    (cons (format "%s (@%s)" name username) id)))

(defun mg--format-favorites-as-candidates ()
  (mapcar
   (lambda (user)
     (let* ((name (car (cdr user)))
            (username (car (cdr (cdr user))))
            (id (mg--to-user-id username)))
       (cons (format "%s (%s)" name username) id)))
   mg-favorite-users))

;;;###autoload (autoload 'magit-gitlab-mr-edit-assignees "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-edit-assignees
 (mr)
 "Edit the assignees of MR."
 (interactive (list (mg--read-mr-scope)))
 (let*
     ((current-reviewers
       (mapcar
        #'mg--format-user-as-candidate (alist-get 'reviewers mr)))
      (current-assignees
       (mapcar
        #'mg--format-user-as-candidate (alist-get 'assignees mr)))
      (candidate-assignees
       (append
        current-reviewers
        current-assignees
        (list (mg--format-user-as-candidate (alist-get 'author mr)))
        (mg--format-favorites-as-candidates)))
      (new-assignees
       (seq-uniq
        (completing-read-multiple
         ;; prompt
         (mg--format
          mr
          "Set assignees (space-separated GitLab usernames) [reviewers: %s]: "
          (if current-reviewers
              (concat
               (string-join (mapcar #'car current-reviewers) ", "))
            "none"))
         ;; table
         candidate-assignees
         nil ;; predicate
         nil ;; require-match
         ;; initial-input
         (if current-assignees
             (concat
              (string-join (mapcar #'car current-assignees)
                           ", ")
              ", ")
           nil))))
      (new-assignees-aux
       (mapcar
        (lambda (selection)
          (or (cdr (assoc selection candidate-assignees)) selection))
        new-assignees)))
   (mg--mr-set-prop-async
    mr 'assignee_ids (mapcar #'mg--to-user-id new-assignees-aux)
    :show-value
    (lambda (_)
      (if new-assignees
          (string-join new-assignees ", ")
        "None")))))

;; TODO: this doesn't make sense as an interactive function -- what to do?
(transient-define-suffix
 mg-mr-assign-to-favorite-set
 (mr)
 "Assign MR to favorite users."
 (interactive (list (mg--read-mr-scope)))
 (if-let (assignees
          (transient-args 'mg-mr-assign-to-favorite))
   ;; (print assignees)
   (mg--mr-set-prop-async
    mr
    'assignee_ids
    (mapcar #'mg--to-user-id assignees)
    :show-value (lambda (_) (string-join assignees ", ")))
   (error "Select a non-empty set of favorites first")))

;;;###autoload (autoload 'magit-gitlab-mr-assign-to-reviewers "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-assign-to-reviewers
 (mr)
 "Let the assignees of MR equals the current set of reviewers."
 (interactive (list (mg--read-mr-scope)))
 (let* ((reviewers
         (if-let (reviewers
                  (alist-get 'reviewers mr))
           (mapcar
            #'mg--format-user-as-candidate (alist-get 'reviewers mr))
           (error "This MR has no reviewers!"))))
   (mg--mr-set-prop-async
    mr
    'assignee_ids
    (mapcar #'cdr reviewers)
    :show-value
    (lambda (_) (string-join (mapcar #'car reviewers) ", ")))))

;;;###autoload (autoload 'magit-gitlab-mr-assign-to-me "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-assign-to-me
 (mr)
 "Assign the MR to the current user (you)."
 (interactive (list (mg--read-mr-scope)))
 (let* ((my-username (concat "@" (ghub--username nil 'gitlab)))
        (my-id (mg--to-user-id my-username)))
   (mg--mr-set-prop-async
    mr
    'assignee_ids
    (list my-id)
    :show-value (lambda (_) my-username))))

;;;###autoload (autoload 'magit-gitlab-mr-assign-to-author "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-assign-to-author
 (mr)
 "Assign the MR to its author."
 (interactive (list (mg--read-mr-scope)))
 (let* ((author-username
         (concat "@" (alist-get 'username (alist-get 'author mr))))
        (author-id (alist-get 'id (alist-get 'author mr))))
   (mg--mr-set-prop-async
    mr
    'assignee_ids
    (list author-id)
    :show-value (lambda (_) author-username))))


;;;###autoload (autoload 'magit-gitlab-mr-edit-reviewers "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-edit-reviewers
 (mr)
 "Edit the set of reviewers of MR."
 (interactive (list (mg--read-mr-scope)))
 (let*
     ((current-reviewers
       (mapcar
        #'mg--format-user-as-candidate (alist-get 'reviewers mr)))
      (current-assignees
       (mapcar
        #'mg--format-user-as-candidate (alist-get 'assignees mr)))
      (candidate-reviewers
       (append
        current-reviewers
        current-assignees
        (list (mg--format-user-as-candidate (alist-get 'author mr)))
        (mg--format-favorites-as-candidates)))
      (new-reviewers
       (seq-uniq
        (completing-read-multiple
         ;; prompt
         (mg--format
          mr
          "Set reviewers (space-separated GitLab usernames) [assignees: %s]: "
          (if current-assignees
              (concat
               (string-join (mapcar #'car current-assignees) ", "))
            "none"))
         ;; table
         candidate-reviewers
         nil ;; predicate
         nil ;; require-match
         ;; initial-input
         (if current-reviewers
             (concat
              (string-join (mapcar #'car current-reviewers)
                           ", ")
              ", ")
           nil))))
      (new-reviewers-aux
       (mapcar
        (lambda (selection)
          (or (cdr (assoc selection candidate-reviewers)) selection))
        new-reviewers)))
   (mg--mr-set-prop-async
    mr 'reviewer_ids (mapcar #'mg--to-user-id new-reviewers-aux)
    :show-value
    (lambda (_)
      (if new-reviewers
          (string-join new-reviewers ", ")
        "None")))))

;;;###autoload (autoload 'magit-gitlab-mr-review-by-assignees "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-review-by-assignees
 (mr)
 "Let the reviewers of MR equals the current set of assignees."
 (interactive (list (mg--read-mr-scope)))
 (let* ((assignees
         (if-let (assignees
                  (alist-get 'assignees mr))
           (mapcar
            #'mg--format-user-as-candidate (alist-get 'assignees mr))
           (error "This MR has no reviewers!"))))
   (mg--mr-set-prop-async
    mr
    'reviewer_ids
    (mapcar #'cdr assignees)
    :show-value
    (lambda (_) (string-join (mapcar #'car assignees) ", ")))))

;;;###autoload (autoload 'magit-gitlab-mr-browse "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-browse
 (mr)
 "Browse the MR of the current BRANCH on GitLab with ‘browse-url’."
 (interactive (list (mg--read-mr-scope)))
 (browse-url (alist-get 'web_url mr)))

;;;###autoload (autoload 'magit-gitlab-mr-browse-kill "magit-gitlab" nil t)
(transient-define-suffix
 mg-mr-browse-kill (mr)
 "Add the URL of the current MR to the kill ring.

Works like ‘mg-mr-browse’, but puts the address in the
kill ring instead of opening it with ‘browse-url’."
 (interactive (list (mg--read-mr-scope)))
 (let* ((web-url (alist-get 'web_url mr)))
   (kill-new web-url)
   (message "Added URL `%s' to kill ring" web-url)))

(defun mg--mr-assign-to-favorite--setup-children (_)
  (transient-parse-suffixes
   'mg-mr-assign-to-favorite mg-favorite-users))

;;;###autoload (autoload 'magit-gitlab-customize-favorites "magit-gitlab" nil t)
(defun mg-customize-favorites ()
  "Customize favorite GitLab users."
  (interactive)
  (customize-variable 'mg-favorite-users))

;;;###autoload (autoload 'magit-gitlab-mr-assign-to-favorite "magit-gitlab" nil t)
(transient-define-prefix
 mg-mr-assign-to-favorite (mr) "Assign MR to a favorite user."
 [["Favorites"
   :if (lambda () mg-favorite-users)
   :setup-children mg--mr-assign-to-favorite--setup-children
   :class transient-column]
  ["Handle favorites" ("C"
    "customize favorites"
    mg-customize-favorites)]]
 ["Actions"
  :if
  (lambda () mg-favorite-users)
  ("S" "set" mg-mr-assign-to-favorite-set)]
 (interactive (list
               (mg--read-mr
                :cache (eq transient-current-command 'mg-mr))))
 (transient-setup 'mg-mr-assign-to-favorite nil nil :scope mr))

;;;###autoload (autoload 'magit-gitlab-mr "magit-gitlab" nil t)
(transient-define-prefix
 mg-mr
 (mr)
 "Act on a GitLab merge request."
 [:description
  (lambda ()
    (let ((mr (oref (transient-prefix-object) scope)))
      (let ((title
             (concat
              (propertize "Act on GitLab merge request "
                          'face
                          'transient-heading)
              (propertize (mg--show-mr mr) 'face 'magit-branch-local)
              ": "
              (format "%s " (alist-get 'title mr))
              (format "[target: %s]"
                      (propertize (alist-get 'target_branch mr)
                                  'face 'magit-branch-remote))))
            (labels
             (format "Labels: [%s]"
                     (string-join (alist-get 'labels mr) ", ")))
            (milestone
             (format "Milestone: %s"
                     (if-let (milestone
                              (alist-get 'milestone mr))
                       (format "%%%d (%s)"
                               (alist-get 'iid milestone)
                               (alist-get 'title milestone))
                       "None"))))
        (concat
         (string-join (list title labels milestone) "\n") "\n"))))
  ["Edit"
   ("t" "title" mg-mr-edit-title)
   ("d" "description" mg-mr-edit-description)
   ("m" "milestone" mg-mr-edit-milestone)
   ;; TODO: since we know the MR we make this either "Draft" or "Undraft"
   ("D" "toggle draft status" mg-mr-toggle-draft)
   ("l" "labels" mg-mr-edit-labels)
   ("T" "target branch" mg-mr-edit-target-branch)]
  [:description
   (lambda ()
     (let ((mr (oref (transient-prefix-object) scope)))
       (format "Assignees [%s]"
               (string-join (mapcar
                             (lambda (user)
                               (concat
                                "@" (alist-get 'username user)))
                             (alist-get 'assignees mr))
                            ", "))))
   ("a a" "edit assignees" mg-mr-edit-assignees)
   ("a m" "assign to me" mg-mr-assign-to-me)
   ("a A" "assign to author" mg-mr-assign-to-author)
   ("a r" "assign to reviewers" mg-mr-assign-to-reviewers)
   ("a f" "assign to favorite" mg-mr-assign-to-favorite)]
  [:description
   (lambda ()
     (let ((mr (oref (transient-prefix-object) scope)))
       (format "Reviewers [%s]"
               (string-join (mapcar
                             (lambda (user)
                               (concat
                                "@" (alist-get 'username user)))
                             (alist-get 'reviewers mr))
                            ", "))))
   ("r r" "edit reviewers" mg-mr-edit-reviewers)
   ("r a" "copy assignees to reviewers" mg-mr-review-by-assignees)]

  ["Actions"
   ("v" "open MR on GitLab" mg-mr-browse)
   ("k" "add MR url to kill ring" mg-mr-browse-kill)]]
 (interactive (list (mg--read-mr)))
 (transient-setup 'mg-mr nil nil :scope mr))

(provide 'magit-gitlab)

;; Local Variables:
;; read-symbol-shorthands: (("mg-" . "magit-gitlab-"))
;; End:

;;; magit-gitlab.el ends here

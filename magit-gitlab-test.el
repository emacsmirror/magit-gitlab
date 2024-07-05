(require 'el-mock)
(require 'magit-gitlab)

(ert-deftest mg-test-url-encode-project-id ()
  (should
   (equal "tezos%2Ftezos" (mg-url-encode-project-id "tezos/tezos")))
  (should (equal "1234" (mg-url-encode-project-id 1234))))

(ert-deftest mg--test-url-mr ()
  (should
   (equal
    "/projects/tezos%2Ftezos/merge_requests/1234"
    (magit-gitlab--url-mr "tezos/tezos" 1234)))
  (should
   (equal
    "/projects/123/merge_requests/456"
    (magit-gitlab--url-mr 123 456))))

(ert-deftest mg--test-get-sync ()
  "Test synchronous version of mg--get."
  (let ((magit-gitlab-GET-cache (make-hash-table :test 'equal)))
    (with-mock
     (stub ghub-request => "18.0")
     (should (equal "18.0" (magit-gitlab--get "/version" nil))))
    ;; Stub each function at most once per with-mock form.
    (with-mock
     (stub ghub-request => "17.0")
     (should (equal "18.0" (magit-gitlab--get "/version" nil)))
     (should
      (equal
       "17.0" (magit-gitlab--get "/version" nil :no-cache t))))))

(ert-deftest mg--test-get-async ()
  "Test asynchronous version of mg--get."
  (let* ((magit-gitlab-GET-cache (make-hash-table :test 'equal))
         (ghub-request-original (symbol-function 'ghub-request))
         (ghub-request-mock-response nil)
         (ghub-request-mock
          (cl-function (lambda (_method
                                _resource
                                _params
                                &key
                                auth
                                forge
                                callback
                                errorback)
                         (ignore auth forge errorback)
                         (funcall callback
                                  ghub-request-mock-response
                                  nil
                                  nil
                                  nil)))))
    (unwind-protect
        (progn
          ;; Setup mock
          (fset 'ghub-request ghub-request-mock)
          ;; Start test
          (setq ghub-request-mock-response "18.0")
          (magit-gitlab--get
           "/version" nil
           :callback
           (lambda (resp _header _status _req)
             (should (equal resp "18.0"))))
          ;; The cached value is returned
          (setq ghub-request-mock-response "17.0")
          (magit-gitlab--get
           "/version" nil
           :callback
           (lambda (resp _header _status _req)
             (should (equal resp "18.0"))))
          ;; Unless no-cache is t
          (magit-gitlab--get
           "/version" nil
           :no-cache t
           :callback
           (lambda (resp _header _status _req)
             (should (equal resp "17.0")))))
      ;; Restore mock
      (fset 'ghub-request ghub-request-original))))

(ert-deftest mg-test-get-user ()
  (should
   (equal
    4414596
    (alist-get 'id (magit-gitlab--get-user "arvidnl" :no-cache t))))
  (should
   (equal
    4414596 (alist-get 'id (magit-gitlab--get-user "arvidnl")))))


(ert-deftest mg--test-get-milestone-of-iid ()
  ""
  (should
   (equal
    4450915
    (alist-get
     'id
     (magit-gitlab--get-milestone-of-iid
      "tezos/tezos"
      325
      :no-cache t))))
  (should
   (equal
    4450915
    (alist-get
     'id (magit-gitlab--get-milestone-of-iid "tezos/tezos" 325)))))


(ert-deftest mg--test-to-user-id ()
  (let ((magit-gitlab-GET-cache (make-hash-table :test 'equal)))
    (should (equal 4414596 (magit-gitlab--to-user-id "@arvidnl")))))

(ert-deftest mg--test-project-of-remote ()
  "Test magit-gitlab--project-of-remote."
  (let ((magit-gitlab-remote-regexps (custom--standard-value 'magit-gitlab-remote-regexps)))
    ;; ssh
    (should
     (equal
      "NAMESPACE/PROJECT"
      (magit-gitlab--project-of-remote
       "git@gitlab.com:NAMESPACE/PROJECT.git")))
    ;; https
    (should
     (equal
      "NAMESPACE/PROJECT"
      (magit-gitlab--project-of-remote
       "https://gitlab.com/NAMESPACE/PROJECT.git")))
    ;; not a gitlab recognized remote
    (should-error
     (magit-gitlab--project-of-remote "https://google.com"))
    ;; custom domains
    (should
     (equal
      "NAMESPACE/PROJECT"
      (magit-gitlab--project-of-remote
       "https://example.gitlab.com/NAMESPACE/PROJECT.git")))
    ;; custom tld
    (should
     (equal
      "NAMESPACE/PROJECT"
      (magit-gitlab--project-of-remote
       "https://example.gitlab.io/NAMESPACE/PROJECT.git")))
    ;; nested projects
    (should
     (equal
      "NAMESPACE/DIR/PROJECT"
      (magit-gitlab--project-of-remote
       "https://example.gitlab.com/NAMESPACE/DIR/PROJECT.git")))))

(ert-deftest mg--test-strip-remote-prefix ()
  (should
   (equal
    "bar"
    (magit-gitlab--strip-remote-prefix "foo/bar")))
  (should
   (equal
    "bar/baz"
    (magit-gitlab--strip-remote-prefix "foo/bar/baz")))
  (should
   (equal
    "baz"
    (magit-gitlab--strip-remote-prefix "baz"))))

;; Local Variables:
;; read-symbol-shorthands: (("mg-" . "magit-gitlab-"))
;; End:

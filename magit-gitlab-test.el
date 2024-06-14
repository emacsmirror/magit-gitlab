(require 'el-mock)
(require 'magit-gitlab)

(ert-deftest mg-test-url-encode-project-id ()
  (should (equal "tezos%2Ftezos" (mg-url-encode-project-id "tezos/tezos")))
  (should (equal "1234" (mg-url-encode-project-id 1234))))

(ert-deftest mg--test-url-mr ()
  (should (equal "/projects/tezos%2Ftezos/merge_requests/1234" (mg--url-mr "tezos/tezos" 1234)))
  (should (equal "/projects/123/merge_requests/456" (mg--url-mr 123 456))))

(ert-deftest mg--test-get-sync ()
  "Test synchronous version of mg--get."
  (let ((mg--GET-cache (make-hash-table :test 'equal) ))
	(with-mock
      (stub ghub-request => "18.0")
      (should (equal "18.0" (mg--get "/version" nil))))
    ;; Stub each function at most once per with-mock form.
    (with-mock
      (stub ghub-request => "17.0")
      (should (equal "18.0" (mg--get "/version" nil)))
      (should (equal "17.0" (mg--get "/version" nil :no-cache t))))))

(ert-deftest mg--test-get-async ()
  "Test asynchronous version of mg--get."
  (let* ((mg--GET-cache (make-hash-table :test 'equal) )
         (ghub-request-original (symbol-function 'ghub-request))
         (ghub-request-mock-response nil)
         (ghub-request-mock (cl-function
                             (lambda (_method _resource _params &key auth forge callback errorback)
                               (ignore auth forge errorback)
                               (funcall callback ghub-request-mock-response nil nil nil)))))
    (unwind-protect
        (progn
          ;; Setup mock
          (fset 'ghub-request ghub-request-mock)
          ;; Start test
          (setq ghub-request-mock-response "18.0")
          (mg--get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; The cached value is returned
          (setq ghub-request-mock-response "17.0")
          (mg--get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; Unless no-cache is t
          (mg--get
           "/version" nil
           :no-cache t
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "17.0")))))
      ;; Restore mock
      (fset 'ghub-request ghub-request-original))))

(ert-deftest mg-test-get-user ()
  (should (equal 4414596 (alist-get 'id (mg--get-user "arvidnl" :no-cache t))))
  (should (equal 4414596 (alist-get 'id (mg--get-user "arvidnl")))))


(ert-deftest mg--test-get-milestone-of-iid ()
  ""
  (should (equal 4450915 (alist-get 'id (mg--get-milestone-of-iid "tezos/tezos" 325 :no-cache t))))
  (should (equal 4450915 (alist-get 'id (mg--get-milestone-of-iid "tezos/tezos" 325)))))


(ert-deftest mg--test-to-user-id ()
  (let ((mg--GET-cache (make-hash-table :test 'equal)))
	(should (equal 4414596 (mg--to-user-id "@arvidnl")))))

;; (mg--get-mr-of-source-branch
;;  "tezos/tezos" "arvid@rename-ci-image-layers" :no-cache t)
;; (hash-table-keys mg--known-milestones)

;; (mg--get-user "arvidnl")


;; Examples of usage:
;; (mg--project-of-remote "git@gitlab.com:NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (mg--project-of-remote "https://gitlab.com/NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (mg--project-of-remote "https://google.com")

;; Local Variables:
;; read-symbol-shorthands: (("mg-" . "magit-gitlab-"))
;; End:

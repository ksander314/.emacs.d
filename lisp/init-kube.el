;;; init-kube.el --- Kubernetes TRAMP integration -*- lexical-binding: t -*-

;; Custom TRAMP method for kubectl exec with context and namespace support.
;; Path syntax: /kube:NAMESPACE.CONTEXT@POD:/path/to/file
;; Example:     /kube:tv-dashboard.dal2.staging@dashboard-testing-abc123:/app/main.go

(with-eval-after-load 'tramp
  (let ((program (expand-file-name "bin/kube-tramp" user-emacs-directory)))
    (add-to-list 'tramp-methods
                 `("kube"
                   (tramp-login-program ,program)
                   (tramp-login-args (("%u") ("%h") ("-it") ("--") ("%l")))
                   (tramp-direct-async ("/bin/sh" "-c"))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-login ("-l"))
                   (tramp-remote-shell-args ("-i" "-c"))))))

(defun my/kube-contexts ()
  "Return list of kubectl contexts."
  (split-string
   (shell-command-to-string "kubectl config get-contexts -o name 2>/dev/null")
   "\n" t))

(defun my/kube-namespaces (context)
  "Return list of namespaces in CONTEXT."
  (split-string
   (shell-command-to-string
    (format "kubectl --context=%s get namespaces -o jsonpath='{.items[*].metadata.name}' 2>/dev/null"
            (shell-quote-argument context)))
   " " t))

(defun my/kube-pods (context namespace)
  "Return list of running pod names in CONTEXT/NAMESPACE."
  (split-string
   (shell-command-to-string
    (format "kubectl --context=%s -n %s get pods --field-selector=status.phase=Running -o jsonpath='{.items[*].metadata.name}' 2>/dev/null"
            (shell-quote-argument context)
            (shell-quote-argument namespace)))
   " " t))

(defun my/kube-find-file ()
  "Open a file on a Kubernetes pod via TRAMP.
Prompts for context, namespace, pod, then lets you browse with completion."
  (interactive)
  (let* ((context (completing-read "Context: " (my/kube-contexts) nil t))
         (namespace (completing-read "Namespace: " (my/kube-namespaces context) nil t))
         (pods (my/kube-pods context namespace))
         (_ (unless pods
              (user-error "No running pods in %s/%s" context namespace)))
         (pod (completing-read "Pod: " pods nil t))
         (prefix (format "/kube:%s.%s@%s:/" namespace context pod)))
    (find-file (read-file-name "File: " prefix))))

(provide 'init-kube)

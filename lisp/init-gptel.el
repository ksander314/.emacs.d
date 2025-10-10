(require-package 'gptel)
(require 'auth-source)

(setq auth-sources '("~/.authinfo"))

(let ((entry (car (auth-source-search :host "api.openai.com" :max 1))))
  (when entry
    (setq gptel-api-key (plist-get entry :secret))))

(provide 'init-gptel)

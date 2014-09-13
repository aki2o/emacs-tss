(require 'tss)
(require 'el-expectations)

(expectations
  (desc "get-server-response when not active")
  (expect nil
    (stub tss--active-p => nil)
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response when failed send string")
  (expect nil
    (stub tss--active-p => t)
    (stub tss--get-process => nil)
    (stub tss--send-string => nil)
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response when timeout")
  (expect nil
    (stub tss--active-p => t)
    (stub tss--get-process => nil)
    (stub tss--send-string => t)
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response config about response")
  (expect t
    (stub tss--active-p => t)
    (stub tss--get-process => nil)
    (stub tss--send-string => nil)
    (tss--get-server-response "symbol 3 2 /tmp/hoge")
    (and (string= tss--json-response-start-char "{")
         (string= tss--json-response-end-char "}")))
  )


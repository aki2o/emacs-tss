(require 'tss)
(require 'el-expectations)

(expectations
  (desc "get-server-response when not active")
  (expect nil
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response when failed send string")
  (expect nil
    (stub tss--get-process => nil)
    (stub process-send-string => nil)
    (add-to-list 'tss-enable-modes major-mode)
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response when timeout")
  (expect nil
    (stub tss--get-process => nil)
    (stub process-send-string => t)
    (add-to-list 'tss-enable-modes major-mode)
    (tss--get-server-response "symbol 3 2 /tmp/hoge"))
  (desc "get-server-response config about response")
  (expect t
    (stub tss--get-process => nil)
    (stub process-send-string => nil)
    (add-to-list 'tss-enable-modes major-mode)
    (tss--get-server-response "symbol 3 2 /tmp/hoge")
    (and (string= tss--json-response-start-char "{")
         (string= tss--json-response-end-char "}")))
  )


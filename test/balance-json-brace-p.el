(require 'tss)
(require 'el-expectations)

(expectations
  (desc "balance-json-brace-p not brace")
  (expect t
    (tss--balance-json-brace-p "hoge" "" ""))
  (desc "balance-json-brace-p open brace")
  (expect nil
    (tss--balance-json-brace-p "{" "{" "}"))
  (desc "balance-json-brace-p close brace")
  (expect t
    (tss--balance-json-brace-p "{}" "{" "}"))
  (desc "balance-json-brace-p imcomplete json object")
  (expect nil
    (tss--balance-json-brace-p "{\"a\":true,\"b\":[" "{" "}"))
  (desc "balance-json-brace-p imcomplete json object 2")
  (expect nil
    (tss--balance-json-brace-p "{\"a\":true,\"b\":[{\"c\":false}" "{" "}"))
  (desc "balance-json-brace-p complete json object")
  (expect t
    (tss--balance-json-brace-p "{\"a\":true,\"b\":[{\"c\":false}]}" "{" "}"))
  (desc "balance-json-brace-p imcomplete json array")
  (expect nil
    (tss--balance-json-brace-p "[{\"c\":false}" "[" "]"))
  (desc "balance-json-brace-p complete json array")
  (expect t
    (tss--balance-json-brace-p "[{\"c\":false}]" "[" "]"))
  (desc "balance-json-brace-p have brace in text")
  (expect nil
    (tss--balance-json-brace-p "{\"a\":true,\"b\":[{\"c\":\"}\"}" "{" "}"))
  (desc "balance-json-brace-p open text")
  (expect nil
    (tss--balance-json-brace-p "{\"a\":true,\"b\":\"}" "{" "}"))
  (desc "balance-json-brace-p close text")
  (expect t
    (tss--balance-json-brace-p "{\"a\":true,\"b\":\"}\"}" "{" "}"))
  (desc "balance-json-brace-p open text with quote value")
  (expect nil
    (tss--balance-json-brace-p "{\"a\":true,\"b\":\"\\\"}" "{" "}"))
  (desc "balance-json-brace-p close text with quote value")
  (expect t
    (tss--balance-json-brace-p "{\"a\":true,\"b\":\"\\\"}\"}" "{" "}"))
  )


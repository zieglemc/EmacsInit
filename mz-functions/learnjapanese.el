(defun jp/replace-with-hiragana(romaji)
  "Converts a word in romanji into hiragana"
  (interactive "sWord to convert: ")
  (setq hiragana-list
        '(("chiwa" "ちは") ("banwa" "ばんは") (" wa " "は") ("pp" "つp")
          ("kk" "つk") ("tt" "つt") ("ss" "つs") ("dd" "つd") (" " "")
          ("kya" "きゃ") ("kyu" "きゅ") ("kyo" "きょ") ("sha" "しゃ")
          ("shu" "しゅ") ("sho" "しょ") ("cha" "ちゃ") ("chu" "ちゅ")
          ("cho" "ちょ") ("nya" "にゃ") ("nyu" "にゅ") ("nyo" "にょ")
          ("hya" "ひゃ") ("hyu" "ひゅ") ("hyo" "ひょ") ("mya" "みゃ")
          ("myu" "みゅ") ("myo" "みょ") ("rya" "りゃ") ("ryu" "りゅ")
          ("ryo" "りょ") ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gyo" "ぎょ")
          ("ja" "じゃ") ("ju" "じゅ") ("jo" "じょ") ("bya" "びゃ") ("byu" "びゅ")
          ("byo" "びょ") ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pyo" "ぴょ") ("ka" "か")
          ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ") ("sa" "さ")
          ("shi" "し") ("su" "す") ("se" "せ") ("so" "そ") ("ta" "た")
          ("chi" "ち") ("tsu" "つ") ("te" "て") ("to" "と") ("na" "な")
          ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の") ("ha" "は") ("hi" "ひ")
          ("fu" "ふ") ("he" "へ") ("ho" "ほ") ("ma" "ま") ("mi" "み") ("mu" "む")
          ("me" "め") ("mo" "も") ("ya" "や") ("yu" "ゆ") ("yo" "よ") ("ra" "ら")
          ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ") ("wa" "わ") ("wo" "を")
          ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご") ("za" "ざ")
          ("ji" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ") ("da" "だ") ("di" "ぢ")
          ("du" "づ") ("de" "で") ("do" "ど") ("ba" "ば") ("bi" "び") ("bu" "ぶ")
          ("be" "べ") ("bo" "ぼ") ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ")
          ("po" "ぽ") ("n" "ん") ("a" "あ") ("i" "い") ("u" "う") ("e" "え")
          ("o" "お")))

  (dolist (current-hiragana hiragana-list )
    (setq romaji
          (replace-regexp-in-string
           (nth 0 current-hiragana)
           (nth 1 current-hiragana)
           romaji)))
  (message "%s" romaji))

(defun jp/japanese-prompt()
  "Prompt for a japanese word in romanji"
  (setq jp/japanese-word (downcase (read-from-minibuffer "Japanese Word/Phrase: "))))

(defun jp/english-prompt()
  "Prompt for a english word in romanji"
  (setq jp/english-word (read-from-minibuffer "English Word/Phrase: ")))

(defun jp/type-prompt()
  "Prompt for a japanese word in romanji"
  (setq jp/word-type (let ((choices '("Noun" "Adjective" "Verb" "Phrase" "Other")))
    (message "%s" (ido-completing-read "Word Type: " choices )))))

(defun jp/grammar-type-prompt()
  "Prompt for a japanese word in romanji"
  (setq jp/grammar-type (let ((choices '("Time" "Sentence Building" "Other")))
    (message "%s" (ido-completing-read "Grammar Type: " choices )))))

(defun jp/definition-prompt()
  "Prompt for a english word in romanji"
  (setq jp/definition (read-from-minibuffer "Definition <[hidden|hint]>: ")))

(defun jp/japanese-get-word(word)
  "Returns a string with both romanji and hiragana"
  (concat word " / " (jp/replace-with-hiragana word)))

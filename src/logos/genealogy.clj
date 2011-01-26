;; this would be competitive in terms of expressiveness

(defrel parent
     '[pam bob]
     '[tom bob]
     '[tom liz])

(extend-rel parent
     '[bob ann]
     '[bob pat]
     '[pat jim])

(defrel female
     'pam 'liz 'pat 'ann)

(defrel male
     'tom 'bob 'jim)

(defrel offspring
     (parent ?x ?y))

(defrel mother
     (parent ?x ?y)
     (female ?y))

(comment
  ;; prolog is so much about syntactical niceties when it comes to searching
  ;; yes you could do this manually but you'd have to write a lot of boiler plate
  ;; also you encode the information in odd places

  (? (female ?X))
  (? (male ?X) (sibling ?X ?Y) (female ?Y))
  )
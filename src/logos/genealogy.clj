;; this would be competitive in terms of expressiveness

(rel parent
     '[pam bob]
     '[tom bob]
     '[tom liz])

(extend-rel parent
     '[bob ann]
     '[bob pat]
     '[pat jim])

(rel female
     'pam 'liz 'pat 'ann)

(rel male
     'tom 'bob 'jim)

(rel offspring
     (parent ?x ?y))

(rel mother
     (parent ?x ?y)
     (female ?y))
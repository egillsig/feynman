(ns feynman.env)

(def init-types
  {"*" [:forall [[:dim-variable 1] [:dim-variable 2]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 2] 1}]]
         [:dimension {[:dim-variable 1] 1 [:dim-variable 2] 1}]]]

   "/" [:forall [[:dim-variable 1] [:dim-variable 2]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 2] 1}]]
         [:dimension {[:dim-variable 1] 1 [:dim-variable 2] -1}]]]

   "+" [:forall [[:dim-variable 1]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 1] 1}]]
         [:dimension {[:dim-variable 1] 1}]]]

   "-" [:forall [[:dim-variable 1]]
        [:function [:product
                    [:dimension {[:dim-variable 1] 1}]
                    [:dimension {[:dim-variable 1] 1}]]
         [:dimension {[:dim-variable 1] 1}]]]

   "==" [:forall [[:type-variable 1]]
         [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   "!=" [:forall [[:type-variable 1]]
         [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   ">" [:forall [[:type-variable 1]]
        [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   "<" [:forall [[:type-variable 1]]
        [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   "<=" [:forall [[:type-variable 1]]
         [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   ">=" [:forall [[:type-variable 1]]
         [:function [:product [:type-variable 1] [:type-variable 1]] [:bool]]]

   "sqrt" [:forall #{[:dim-variable 1]}
           [:function [:product [:dimension {[:dim-variable 1] 2}]]
            [:dimension {[:dim-variable 1] 1}]]]

   "m" [:dimension {"m" 1}]})

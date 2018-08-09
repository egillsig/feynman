(ns feynman.env)

(def init-types
  {:types {"Bool" :Bool}
   "*" [:forall #{'d__1 'd__2}
        [:function [:product
                    [:dimension {'d__1 1}]
                    [:dimension {'d__2 1}]]
         [:dimension {'d__1 1 'd__2 1}]]]

   "/" [:forall #{'d__1 'd__2}
        [:function [:product
                    [:dimension {'d__1 1}]
                    [:dimension {'d__2 1}]]
         [:dimension {'d__1 1 'd__2 -1}]]]

   "+" [:forall #{'d__1}
        [:function [:product [:dimension {'d__1 1}] [:dimension {'d__1 1}]]
         [:dimension {'d__1 1}]]]

   "-" [:forall #{'d__1}
        [:function [:product [:dimension {'d__1 1}] [:dimension {'d__1 1}]]
         [:dimension {'d__1 1}]]]

   "^" [:function [:product [:dimension {}] [:dimension {}]] [:dimension {}]]

   "==" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]
   "!=" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]
   ">" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]
   "<" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]
   "<=" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]
   ">=" [:forall #{'alpha__1} [:function [:product 'alpha__1 'alpha__1] :Boolean]]

   "sqrt" [:forall #{'d__} [:function [:product [:dimension {'d__ 2}]]
                            [:dimension {'d__ 1}]]]})

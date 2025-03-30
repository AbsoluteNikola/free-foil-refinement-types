
type list('a) =
  | Nil
  | Cons(_:'a, _:list('a))
  ;

/*@ val fold_right : _:(_:'alice => _:'bob => 'bob) => _:'bob => _:list('alice) => 'bob */
let rec fold_right = (f, b, xs) => {
  switch (xs) {
    | Nil        => b
    | Cons(h, t) => let res = fold_right(f, b, t);
                    f(h, res)
  }
};
0

/*M len : _:list('a) => int */

type list('a) =
  | Nil                      => [v| len(v) == 0]
  | Cons (x:'a, xs:list('a)) => [v| len(v) == 1 + len(xs)]
  ;

/*@ val singleton : _:'a => list('a)[v|len(v) == 10] */
let singleton = (x) => {
  let t = Nil;
  Cons(x, t)
};
0

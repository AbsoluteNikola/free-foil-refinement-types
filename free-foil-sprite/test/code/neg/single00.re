
type list('a) =
  | Nil
  | Cons(_:'a, _:list('a))
  ;

/*@ val singleton : _:int => list(int[v|0 <= v]) */
let singleton = (x) => {
  let t = Nil;
  Cons(x, t)
};
0

/**@ qualif pos(v:int):        (0 <= v) */
/**@ qualif geq(v:int, n:int): (n <= v) */

/*@ val foldn : _:(_:'a => _:int[?] => 'a) => _:'a => i:int[?] => n:int[?] => 'a */
let rec foldn = (f, acc, i, n) => {
  let leq = i < n;
  if (leq) {
    let ip = i + 1;
    let accp = f(acc, i);
    foldn(f, accp, ip, n)
  } else {
    acc
  }
};

/*@ val add : x:int => y:int => int[v|v == x + y] */
let add = (x, y) => {
  x + y
};

/*@ val main : m:int[v|0<=v] => int[v|0<=v] */
let main = (m) => {
  let zero = 0;
  foldn(add, zero, zero, m)
};

main(10)


/*@ val choose : _:'a => _:'b => 'a */
let choose = (x, y) => { x };

/*@ val check : _:int[v|0<=v] => _:int[v|0<=v] => int[v|0<=v] */
let check = (a, b) => {
  let aM  = a - 1;
  let res = choose(aM, a);
  res
};
check(1, 2)

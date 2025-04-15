
/*@ val choose : _:'a => _:'b => 'b */
let choose = (x, y) => { y };

/*@ val check : _:int[v|0<=v] => _:int[v|0<=v] => int[v|0<=v] */
let check = (a, b) => {
  let aM  = a - 1;
  let res = choose(a, aM);
  res
};
check(1, 2)

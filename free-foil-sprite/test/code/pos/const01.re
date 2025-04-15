
/*@ val choose : _:'a => _:'b => 'a */
let choose = (x, y) => { x };

/*@ val check : x:int[v|0<=v] => _:int[v|0<=v] => int[v|0<=v] */
let check = (a, b) => {
  let aP  = a + 1;
  let aM  = a - 1;
  let res = choose(aP, aM);
  let res2 = choose(true, false);
  res
};
0

/*@ val const : _:'a => (_: 'b => 'a )*/
let const = (x) => { (y) => {
    x
}};

/*@ val check1 : x:int[v|0<=v] => int[v|0<=v] */
let check1 = (y) => {
  let r1 = const(y);
  let r2 = r1(false);
  r2
};
0

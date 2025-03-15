/*@ val const : _:'a => _: 'b => 'a */
let const = (x, y) => {
    x
};

/*@ val check1 : x:int[v|0<=v] => int[v|0<=v] */
let check1 = (x) => {
  let r1 = const(x, false);
  r1
};
0

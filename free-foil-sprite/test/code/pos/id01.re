/*@ val id : x:'a => (y:int => (z:'b => 'a)) */
let id = (x) => {(y) => { (z) => { x }}};

/*@ val id2 : x:'a => 'a */
let id2 = (x) => {
    let i1 = id(x);
    let i2 = i1(0);
    i2(0)
};

/*@ val check1 : x:int[v|0<=v] => int[v|0<=v] */
let check1 = (y) => {
  let i1 = id(y);
  let i2 = i1(0);
  let res1 = i2(false);
  res1
};
0

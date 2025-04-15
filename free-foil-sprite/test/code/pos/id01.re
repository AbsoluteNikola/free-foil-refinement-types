/*@ val id : x:'a => y:int => z:'b => 'a */
let id = (x, y, z) => { x };

/*@ val id2 : x:'a => 'a */
let id2 = (x) => {
    let i1 = id(x, 0, 0);
    i1
};

/*@ val check1 : x:int[v|0<=v] => int[v|0<=v] */
let check1 = (y) => {
  let i1 = id(y, 0, false);
  i1
};
0

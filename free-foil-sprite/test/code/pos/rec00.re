/*@ val sum : n:int[n|true] => int[v|true] */
let rec sum = (n) => {
   let z = n - 1;
   sum(z)
};
sum(10)

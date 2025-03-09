/*@ val sum : n:int => int */
let rec sum = (n) => {
   let z = n - 1;
   sum(z)
};
sum(10)

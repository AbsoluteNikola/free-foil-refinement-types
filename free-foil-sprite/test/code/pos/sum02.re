/*@ val sum : x:int => y:int => z:int => int[v| v == ((x + y) + z)] */
let rec sum = (x, y, z) => {
    let s1 = x + y;
    let s2 = s1 + z;
    s2
};
sum(0, 1, 2)

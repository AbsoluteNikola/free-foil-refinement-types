

/*@ val inc: x:int => int[v|v == x + 1] */
let inc = (x) => {
    x + 1
};

/*@ val foo: int => int => int[v|v == 0] */
let foo = (x) => {
    (y) => {
        0
    }
};

let bar = inc(10);
bar

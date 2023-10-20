console.time("Elapsed:");
let count = 0;
let sum = 0;

for (let i = 0; i < 1000; i++) {
  sum = sum+ i;
  count++;
}

console.log(count + "-" + sum);

console.timeEnd("Elapsed:");

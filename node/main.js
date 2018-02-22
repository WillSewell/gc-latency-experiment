const windowSize = 200000;
const msgCount = 1000000;
let worst = -Infinity;

function mkMessage(n) {
  return new Uint8Array(1024).fill(n % 255);
}

function pushMsg(c, highID) {
  const start = Date.now();
  const m = mkMessage(highID);

  c[highID % windowSize] = m;

  const elapsed = Date.now() - start;

  if (elapsed > worst) {
    worst = elapsed;
  }
}

function main() {
  const c = [];

  for (let i = 0; i < msgCount; i++) {
    pushMsg(c, i);
  }

  console.log(`Worst push time: ${worst}ms`);
}

main();

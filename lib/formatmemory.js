const clc = require("cli-color");
const memoryLength = 16

const LINELENGTH = 48;

const formatMemory = mem => {
  // 1.     get the full string
  // 1.1.   remove to large variables
  let variables = []
  let charArr = mem
  .map(([pos, size, val]) => {
    if(val.length >= (size*3/2)) {
      variables.push(val);
      val = "$"+(variables.length - 1);
    }
    return [pos, size, val];
  })
  // 1.2.   resize to small variables
  .map(([pos, size, val]) => {
    let isSmall = val.length < size;
    let isNamedVar   = val[0].toLowerCase() != val[0];
    let isMetaVar    = val[0] === "$";
    if( isSmall || isNamedVar || isMetaVar ) {
      let padLength = size - val.length +  Math.ceil(size/2)-1;
      let leftPadLength  = Math.floor(padLength/2)
      let rightPadLength = padLength - leftPadLength;
      val = ['_'.repeat(leftPadLength) + clc.yellow(val) + '_'.repeat(rightPadLength)];
      return val;
    } else {
    // simple case
      return val.split("");
    }
  })
  .reduce((a,e) => a.concat(e), [])

  // 2.     combine lines
  let lines = []
  let line = "";
  let odd = false;
  for(let i = 0; i < charArr.length; i++ ) {
    line += charArr[i];
    if(odd || charArr[i].length > 1) line += " ";
    odd = !odd;
    if(charArr[i].length > 1) odd = false;
    if(clc.getStrippedLength(line) >= LINELENGTH) {
      lines.push(clc.slice(line, 0, LINELENGTH));
      line = clc.slice(line, LINELENGTH);
    }
  }

  // 3. more formatting
  let gt = i => {
    let str = (i * 16).toString(16)
    if(str.length % 2 === 1) str = "0" + str;
    return clc.xterm(244)(str);
  }
  let gv = i => i in variables && "$"+i+" - "+variables[i] || "";
  lines.push(line + " ".repeat(LINELENGTH - clc.getStrippedLength(line)))
  lines = lines.map((line, i) => gt(i) + "  " + line + "    " + gv(i))

  let title  = "MEMORY" + "_".repeat( LINELENGTH ) + "\n"
  let header = "    00 01 02 03 04 05 06 07 08 09 0a b0 0c 0d 0e 0f\n"
  let hr     = "    -----------------------------------------------\n"

  return clc.xterm(244)(header + hr) + lines.join("\n")
}

const exampleMemory2 = (a, b) => [
  [0,  4, "0000"],
  [4,  2, "aa"],
  [8,  (a+1)*2, "V"+"a".repeat(b)],
  [10, 2, "cc"],
  [12, 2, "cc"],
  [14, 2, "cc"],
  [16, 2, "cc"],
  [18, 2, "cc"],
  [20, 2, "cc"],
  [22, 38, "ccdd1122334455667788991011121314151617"]
]


// let as = 3;
// let bs = 8;
// for(let i = 0; i < as * bs; i++ ) {
// let a = Math.floor(i/bs);
// let b = i % bs;
// console.log(formatMemory(exampleMemory2(a, b))+"\n");
// }
if(process.argv[1] === __filename) {
  console.log(formatMemory(exampleMemory2(0, 1))+"\n");
}

// console.log(`
// STACK_______________________________________________________________
// 00  2334274828868486838236593658492658491746571940583827165452123647
// `);
//
//
// console.log(`
// MEMORY:____________________________________________
//     00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
// 00  00 00 _________Address__________ ac dd e3 5a de
// 10  4a fe
// `);
//
//
// console.log(`
// MEMORY:____________________________________________
//     00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f
// 00  00 00 $1 ac dd e3 5a de                          $1 .. LongVariable
// `);
//
module.exports = {
  formatMemory
};

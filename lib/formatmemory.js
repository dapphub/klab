const clc = require("cli-color");
const memoryLength = 16
const {
  read
} = require("../lib/util.js");
const kast = require("./kast.js");

const LINELENGTH = 48;

const formatMemory = mem => {
  // 1.     get the full string
  // 1.1.   remove to large variables
  let variables = []
  let charArr = mem
  .map(([pos, size, val]) => {
    if(val.length >= (size*3)) {
      variables.push(val);
      val = "$"+(variables.length - 1);
    }
    return [pos, size, val];
  })
  // 1.2.   resize to small variables
  .map(([pos, size, val]) => {
    let isSmall = val.length < size * 2;
    let isNamedVar   = val[0].toLowerCase() != val[0];
    let isMetaVar    = val[0] === "$";
    if( isSmall || isNamedVar || isMetaVar ) {
      let padLength = size * 3/2 - val.length -1;
      let leftPadLength  = Math.max(Math.floor(padLength/2), 0)
      let rightPadLength = Math.max(padLength - leftPadLength, 0);
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
    // if(str.length % 2 === 1) str = "0" + str;
    str = "0".repeat(4 - str.length % 4) + str
    return clc.xterm(244)(str);
  }
  let gv = i => i in variables && "$"+i+" - "+variables[i] || "";
  lines.push(line + " ".repeat(LINELENGTH - clc.getStrippedLength(line)))
  lines = lines.map((line, i) => gt(i) + "  " + line + "    " + gv(i))

  let title  = "MEMORY" + "_".repeat( LINELENGTH ) + "\n"
  let header = "      00 01 02 03 04 05 06 07 08 09 0a b0 0c 0d 0e 0f\n"
  let hr     = "      -----------------------------------------------\n"

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

// TODO - handle nonstandard memory
const prepareMem = json => {
  if(json.node == "KApply" && json.label == "_Map_") {
    var mem = json.args
      .reduce(({arr, top}, cell) => {
        if(!top && cell.args[1].label == "nthbyteof" && cell.args[1].args[1].token == "0" && cell.args[1].args[2].token == "32") {
          return {
            arr,
            top: [parseInt(cell.args[0].token), 1, kast.format(cell.args[1].args[0])]
          };
        } else if(!top && cell.args[1].label == "nthbyteof") {
          console.log(top, cell);
          throw new Error("unstandard memory, plz help, denis!");
        } else if(!top && cell.args[1].node == "KToken" && cell.args[1].sort == "Int") {
          let token = parseInt(cell.args[1].token).toString(16);
          return {
            arr: arr.concat([[parseInt(cell.args[0].token), 1, token.length == 1 ? "0" + token : token]])
          };
        } else if(top && cell.args[1].label == "nthbyteof" && kast.format(cell.args[1].args[0]) == top[2]) {
          // TODO - check length
          return {
            arr,
            top: [top[0], top[1] + 1, top[2]]
          }
        } else if(top && cell.args[1].label == "nthbyteof" && kast.format(cell.args[1].args[0]) != top[2]) {
          return {
            arr: arr.concat([top]),
            top: [parseInt(cell.args[0].token), 1, kast.format(cell.args[1].args[0])]
          }
        } else if(top && cell.args[1].node == "KToken" && cell.args[1].sort == "Int") {
          let token = parseInt(cell.args[1].token).toString(16);
          return {
            arr: arr.concat([top]).concat([[parseInt(cell.args[0].token), 1, token.length == 1 ? "0" + token : token]])
          }
        } else {
          console.log(top, cell);
          throw new Error("unstandard memory layout, help, denis2")
        }
        // todo top and int case
        //
      }, {arr: []})
    if(mem.top) {
      mem = mem.arr.concat([mem.top]);
    } else {
      mem = mem.arr;
    }
    mem = mem.sort((a,b) => a[0] - b[0])

    const mem_full = mem.reduce(({arr, pos}, cell) => {
      if(cell[0] == pos) {
        return {
          arr: arr.concat([cell]),
          pos: pos + cell[1]
        };
      } else {
        let emp = [];
        for(var i=0; i < cell[0] - pos; i++) {
          emp.push([pos + i, 1, "  "])
        }
        return {
          arr: arr.concat(emp).concat([cell]),
          pos: cell[0] + cell[1]
        };
      }
    }, {arr: [], pos: 0})

    mem = mem_full.arr;
    return mem;
  } else if(json.node == "KApply" && json.label == ".Map") {
    return [];
  }
}


// let as = 3;
// let bs = 8;
// for(let i = 0; i < as * bs; i++ ) {
// let a = Math.floor(i/bs);
// let b = i % bs;
// console.log(formatMemory(exampleMemory2(a, b))+"\n");
// }
if(process.argv[1] === __filename) {
  const json_path = "/home/mhhf/src/k-dss/mem/18ebf5866e9ab18fdbe6.json";
  const json = JSON.parse(read(json_path));

    // console.log(JSON.stringify(mem, false, 2));
  console.log(formatMemory(prepareMem(json))+"\n");
    // console.log(JSON.stringify(mem, false, 2));
  // console.log(formatMemory(exampleMemory2(0, 1))+"\n");
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
  formatMemory,
  prepareMem
};

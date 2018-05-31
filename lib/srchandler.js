const fs = require("fs");
const clc = require('cli-color');
const tw = process.stdout.columns; // total width

// const source_path = "/Users/mhhf/dh/verified-smart-contracts/dappsys/exp-squaring-percise/out/ExpSquaringPercise.sol.json";
// const contract_name = "exp";
// const src_filepath = "/Users/mhhf/dh/verified-smart-contracts/dappsys/exp-squaring-percise/src/ExpSquaringPercise.sol"; // TODO - get rid of this
// const contract_search_regex = new RegExp("\\:" + contract_name);
//
// const source_json = JSON.parse(fs.readFileSync(source_path));
//
// const contract_location = Object.keys(source_json.contracts).find(s => contract_search_regex.test(s));
//
// const bin_string = source_json.contracts[contract_location]["bin-runtime"];
// const srcmap = source_json.contracts[contract_location]["srcmap-runtime"];
// const source = fs.readFileSync(src_filepath).toString();
// const bin_string = fs.readFileSync("/Users/mhhf/dh/verified-smart-contracts/sic/T0-runtime.evm").toString();
// const srcmap = fs.readFileSync("/Users/mhhf/dh/verified-smart-contracts/sic/rpow.srcmap").toString().split("\n").join(";");
// const source = fs.readFileSync("/Users/mhhf/dh/verified-smart-contracts/sic/rpow.src").toString();

// HELPER
const hex = i => (s = i.toString(16)).length == 1 ? "0" + s : s;



// EVM Instructions
const evm_i = fs.readFileSync(__dirname + "/evm.csv")
  .toString()
  .split("\n")
  .filter(s => s !== "")
  .map(s => s.split(" "))
  .reduce((a, e) => (a[e[0]] = e[1]) && a, {})

// BIN runtime
// var code_arr = []
// for(let i=0; i<bin_string.length; i+=2) {
//   code_arr.push(parseInt(bin_string.slice(i, i+2), 16));
// }

// SRC Map
// var last = [0, 0, 0, 0];
// const srcmapArr = srcmap
//   .split(";")
//   .map(e => {
//     console.log(e);
//     if(e === "") return last
//     let tmp = e.split(":")
//       .map(o => parseInt(o))
//       .forEach((o, i) => {
//         if(typeof o === "number" && o > 0) last[i] = o;
//       });
//     return last.slice();
//   })
//   // .map(e => e.split(":").map(o => parseInt(o)))
const genSrcmapArr = srcmap => srcmap
  .split(";")
  .map(l => l.split(":"))
  .map(([s, l, f, j]) => ({ s: s === "" ? undefined : s, l, f, j }))
  .reduce(
    ([last, ...list], { s, l, f, j }) => [
      {
        s: parseInt(s || last.s, 10),
        l: parseInt(l || last.l, 10),
        f: parseInt(f || last.f, 10),
        j: j || last.j
      },
      last,
      ...list
    ],
    [{}]
  )
  .reverse()
  .slice(1)

// const bla = srcmap.split(";")


// Instructions
const get_pc_to_inst_map = bin_string => {
  const code_arr = []
  for(let i=0; i<bin_string.length; i+=2) {
    code_arr.push(parseInt(bin_string.slice(i, i+2), 16));
  }
  const pc_to_inst_map = [];
  const instructions = [];
  let i = 0;
  while(i < code_arr.length) {
    let i_ = i;
    let index = hex(code_arr[i]);
    let instruction_string = evm_i[index] || `UNKNOWN (${index})`;

    if(code_arr[i] >= 96 && code_arr[i] <= 127) {
      let length = code_arr[i] - 95;
      instruction_string += " " + code_arr.slice(i + 1, i + 1 + length).map(s => hex(s)).join("");
      i += length;
    }
    i++;
    instructions.push(instruction_string);
    for(let j=i_; j < i; j++) {
      pc_to_inst_map.push(instructions.length - 1);
    }
  }
  return {
    pc_to_inst_map,
    instructions
  };
}

// bla.forEach((s,i) => console.log(i.toString(16), s))


// EXPORT
const getCodeStringFromPc = (config, pc, trim, pc_to_inst_map, srcmapArr) => {
  let hide_color = 244;
  let show_color = 255;
  if(!pc_to_inst_map) {
    pc_to_inst_map = get_pc_to_inst_map(config.bin_runtime)
      .pc_to_inst_map;
  }
  // console.log("pc", pc, "inst", pc_to_inst_map[pc], "srcmap", srcmapArr[pc_to_inst_map[pc]]);
  if(!srcmapArr) {
    srcmapArr = genSrcmapArr(config.sourcemap);
  }
  var cpos = srcmapArr[pc_to_inst_map[pc]];
  // console.log(pc.toString(16), pc_to_inst_map[pc].toString(16), cpos.s, cpos.l, bla[pc_to_inst_map[pc]]);
  let hide = str => clc.xterm(hide_color)(str.split("\n").join("\n"))
  let show = str => clc.xterm(show_color)(str.split("\n").join("\n"))

  let pre  = config.source.slice(0, cpos.s);
  let root = config.source.slice(cpos.s, cpos.s + cpos.l);
  let post = config.source.slice(cpos.s + cpos.l)
  if(trim && pre.split("\n").length > 5) pre = pre.split("\n").slice(-5).join("\n")
  if(trim && root.split("\n").length > 5) root = root.split("\n").slice(0, 3).concat(["[...]"]).concat(root.split("\n").slice(-2)).join("\n")
  if(trim && post.split("\n").length > 5) post = post.split("\n").slice(0, 5).join("\n")

  var str = (hide(pre) + show(root) + hide(post));
  str = str.split("\n").map(s => "  " + s + " ".repeat(Math.max(0, tw - 2 - clc.getStrippedLength(s)))).join("\n");
  // str = clc.bgXterm(236)(str);
  return str;
}


// pc_to_inst_map
// .forEach((inst, i) => {
//   if(inst >= srcmapArr.length) return null;
//   let s = srcmapArr[inst];
//   console.log(i + " " + inst + " " + instructions[inst] + " " + s.s + " " + s.l);
//   // console.log(source.slice(s.s, s.s + s.l));
//   console.log(getCodeStringFromPc(i));
// })
module.exports = {
  getCodeStringFromPc,
  get_pc_to_inst_map,
  genSrcmapArr
  // instructions,
  // pc_to_inst_map
};

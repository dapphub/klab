const fs = require("fs");
const clc = require('cli-color');
const tw = process.stdout.columns; // total width
// TODO - make good source code stuff


// HELPER
const hex = i => (s = i.toString(16)).length == 1 ? "0" + s : s;

// EVM Instructions
const evm_i = fs.readFileSync(__dirname + "/evm.csv")
  .toString()
  .split("\n")
  .filter(s => s !== "")
  .map(s => s.split(" "))
  .reduce((a, e) => (a[e[0]] = e[1]) && a, {})

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
  if(trim && pre.split("\n").length > 4) pre = pre
    .split("\n")
    .slice(-2)
    .join("\n")
  if(trim && root.split("\n").length > 4) root = root
    .split("\n")
    .slice(0, 2)
    .concat(["[...]"])
    .concat(root.split("\n").slice(-2))
    .join("\n")
  if(trim && post.split("\n").length > 4) post = post
    .split("\n")
    .slice(0, 2)
    .join("\n")

  var str = ("...\n  " + hide(pre) + show(root) + hide(post) + "\n  ...");
  str = str.split("\n").map(s => "  " + s ).join("\n");
  // str = clc.bgXterm(236)(str);
  return str;
}


module.exports = {
  getCodeStringFromPc,
  get_pc_to_inst_map,
  genSrcmapArr
};

const parse = {
  init: data => ({data: data[1]}),
  target: data => ({data: data[1]}),
  srstep: data => {
    let d = data[2].split("_");
    return {
      type: "rstep",
      from: d[0]+"_"+d[1],
      to: d[2]+"_"+d[3],
      rule: data[0]
    }
  },
  finished: data => {
    return {
      nodeid: data[0]
    }
  },
  implication: data => ({
    nodeid: data[0],
    lhs: data[1].split("_")[0],
    rhs: data[1].split("_")[1]
  }),
  z3result: data => ({
    nodeid: data[1],
    query: data[0],
    result: data[2]
  }),
  // step: data => {
  //   let d = data[1].split("_");
  //   return {
  //     from: d[0]+"_"+d[1],
  //     to: d[2]+"_"+d[3]
  //   }
  //   console.log("step");
  // },
  rstep: data => {
    let d = data[2].split("_");
    return {
      from: d[0]+"_"+d[1],
      to: d[2]+"_"+d[3],
      rule: data[0]
    }
  },
  close: data => {
    return ({})
  }
}


module.exports = msg => {
  let data = msg.data.split(" ").slice(1);
  return data[0] in parse
    && Object.assign({type: data[0]}, parse[data[0]](data.slice(1)))
    || {type: "unknown", data}
}


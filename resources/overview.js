const h = n => s => `<${Array.isArray(n) ? n.join(' ') : n}>${Array.isArray(s) && s.join('') || s}</${Array.isArray(n) ? n[0] : n}>`
const table = h("table")
const tr = h("tr")
const td = h("td")
const span = h("span")
const div = h("div")
const date = date => {
  const d = new Date(date);
  const str = `${d.getFullYear()}-${d.getMonth()+1}-${d.getDate()} ${d.getHours()}:${d.getMinutes()}`
  return h(["span", 'class="date"'])(str)
}

const status = map => h(["div", 'clsas="status"'])(h(['table', 'class="status"'])(map.map(row => tr(row.map(span).map(td)))))

const accepted = num => h(["span", 'class="accepted"'])([
  span("accepted:"),
  span(num)
])
const rejected = num => h(["span", 'class="rejected"'])([
  span("rejected:"),
  span(num)
])

document.getElementById("main").innerHTML = 'loading'

function loadJSON(callback) {
  var xobj = new XMLHttpRequest();
  xobj.overrideMimeType("application/json");
  xobj.open('GET', 'report.json', true); // Replace 'my_data' with the path to your file
  xobj.onreadystatechange = function () {
    if (xobj.readyState == 4 && xobj.status == "200") {
      // Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
      callback(JSON.parse(xobj.responseText));
    }
  };
  xobj.send(null);
}

loadJSON(json => {
  const url = json.url;
  delete json.url;
  const name = json.name;
  delete json.name;

  const process_proofs = proofs => {
    const prooflist = Object.keys(proofs)
      .map(fabiid => {
        const fabi_proofs = proofs[fabiid];
        return fabi_proofs.proofs.map(p => ({...p, proof_name: fabiid}))
      })
      .reduce((a, b) => a.concat(b), [])

    const grouped_proofs = prooflist
      .reduce((a, o) => {
        // if(!(o.status in a)) a[o.status] = [];
        a[o.status] = (a[o.status] || []).concat(o);
        return a;
      }, {})
    return {
      prooflist,
      grouped_proofs
    };
  }

  const m = Object.keys(json)
    .map(hash => ({
      ...json[hash],
      hash
    }))
    .sort((a, b) => (new Date(b.date) - new Date(a.date)))
    .filter((o, i) => {
      const {
        prooflist,
        grouped_proofs
      } = process_proofs(o.proofs)

      const accepted_num = (grouped_proofs.accept || []).length;
      const rejected_num = (grouped_proofs.reject || []).length;

      return i == 0 || (!grouped_proofs.running) && (accepted_num > 0 || rejected_num > 0)
    })

  const html = m
    .map((o, i) => {
      const {
        prooflist,
        grouped_proofs
      } = process_proofs(o.proofs)

      const accepted_num = (grouped_proofs.accept || []).length;
      const rejected_num = (grouped_proofs.reject || []).length;
      const total_num    = prooflist.length;
      const running = !!grouped_proofs.running;
      const skip = (grouped_proofs.running || [])
        .concat(grouped_proofs.queue || [])
        .map(o => o.spec)

      var diff = '';
      if(i == 0 && m.length > 1) {
        const diff_basis = process_proofs(m[1].proofs)
        const types = ['accept', 'reject']
        const do_diff = type => {
          const da = (diff_basis.grouped_proofs[type] || [])
            .map(o => o.spec)
          const db = (grouped_proofs[type] || [])
            .map(o => o.spec)

          const all = Object.keys(da.concat(db)
            .reduce((a, o) => {
              a[o] = true;
              return a;
            }, {}))

          const diffstr = all
            .filter(spec => !(da.indexOf(spec) > -1 && db.indexOf(spec) > -1))
            .filter(spec => skip.indexOf(spec) == -1)
            .map(spec => {
              const pol = da.indexOf(spec) > -1
              const prefix = (pol ? "- " : "+ ")
              return h(["div", `class="change ${pol ? "neg" : "pos"}"`])(prefix + spec)
            })

          return diffstr;
        }
        const accepted_diff = do_diff('accept').join('')
        const rejected_diff = do_diff('reject').join('')

        diff = h(['div', 'class="diff"'])([
          div([
            h(['span', 'class="title"'])('accept'),
            accepted_diff
          ]),
          div([
            h(['span', 'class="title"'])('reject'),
            rejected_diff
          ])
        ])
      }


      return h(["div", 'class="proof"'])([
        h(["div", 'class="proofinfo"'])([
          h(["div", 'class="link"'])([
            h(['a', 'class="build-hash"', `href="${o.hash}"`])(o.hash),
            h(['span', 'class="git-hash"'])(`git: ${o.git}`),
            h(['div', 'class="info"'])([
              date(o.date),
              h(['span', 'class="running"'])(running ? 'running (' + skip.length + ')' : '')
            ])
          ]),
          status([
            ["total", total_num],
            ["accepted", accepted_num],
            ["rejected", rejected_num]
          ])
        ]),
        diff
      ])
    }).join('')

  document.getElementById("main").innerHTML = h(['div', 'class="proofs"'])(html);
})

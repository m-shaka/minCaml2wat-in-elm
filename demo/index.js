import libwabt from 'wat2wasm/libwabt'
import { Elm } from './src/Main.elm'

const app = Elm.Main.init({ node: document.getElementById('elm') })

app.ports.sendWat.subscribe(wat => {
    libwabt()
        .then(wabt => {
            const module = wabt.parseWat('', wat, {})
            const binary = module.toBinary({ log: true }).buffer
            WebAssembly.instantiate(binary, {})
                .then(results => {
                    const result = results.instance.exports.exported_main()
                    app.ports.returnValue.send(result.toString())
                })
        })
})

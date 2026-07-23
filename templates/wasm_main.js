import { WASI, OpenFile, File, ConsoleStdout } from '@bjorn3/browser_wasi_shim';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

async function startWasm() {
    try {
        const fds = [
            new OpenFile(new File([])), // stdin
            ConsoleStdout.lineBuffered(msg => console.log(msg)), // stdout
            ConsoleStdout.lineBuffered(msg => console.error(msg)) // stderr
        ];

        const wasi = new WASI([], [], fds, { debug: false });
        const wasiImport = { ...wasi.wasiImport };
        
        // Prevent termination on proc_exit
        wasiImport.proc_exit = (code) => {
            if (code !== 0) {
                console.error(`WASM process exited with code ${code}`);
            }
        };

        // JS FFI needs a proxy to resolve GHC exports before the instance is created
        let wasmExports;
        const exportsProxy = new Proxy({}, {
            get: (_, prop) => {
                if (!wasmExports) throw new Error("Exports not ready");
                return wasmExports[prop];
            }
        });

        const jsffi = ghc_wasm_jsffi(exportsProxy);
        const importObject = {
            wasi_snapshot_preview1: wasiImport,
            ghc_wasm_jsffi: jsffi
        };

        const response = await fetch("/{{WASM_NAME}}");
        const realInstance = (await WebAssembly.instantiateStreaming(response, importObject)).instance;
        wasmExports = realInstance.exports;

        // Initialize WASI and GHC RTS
        wasi.initialize(realInstance);
        
        if (!realInstance.exports.hs_init) {
            throw new Error("GHC RTS initialization function (hs_init) not exported");
        }
        realInstance.exports.hs_init(0, 0);

        if (!realInstance.exports.app_init) {
            throw new Error("Application initialization function (app_init) not exported");
        }
        // Define global helpers BEFORE app_init() so alien effects
        // (keylistener, inputfield, etc.) can use them immediately.
        window.dispatch = (rawMessage) => {
            try {
                if (typeof rawMessage !== 'string') {
                    console.error('[harpe] dispatch: expected string, got', typeof rawMessage, rawMessage);
                    return;
                }
                if (realInstance.exports.app_dispatch) {
                    const msgId = jsffi.newJSVal(rawMessage);
                    realInstance.exports.app_dispatch(msgId);
                    jsffi.freeJSVal(msgId);
                }
            } catch (err) {
                console.error('[harpe] dispatch failed:', err, '| rawMessage:', rawMessage);
            }
        };

        window.inform = (constructorName, ...args) => {
            try {
                const escapeStr = (s) => typeof s === 'string' ? '"' + s.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"' : s;
                const serializedArgs = args.map(escapeStr).join(' ');
                const rawMessage = `MsgParent (${constructorName}${serializedArgs ? ' ' + serializedArgs : ''})`;
                window.dispatch(rawMessage);
            } catch (err) {
                console.error('[harpe] inform failed:', err, '| constructorName:', constructorName, '| args:', args);
            }
        };

        window.addInformer = (id, eventType, selector, callback) => {
            try {
                const handler = (event) => {
                    try {
                        if (selector) {
                            const targetEl = document.querySelector(selector);
                            const isMouseEvent = ['click', 'input', 'change', 'submit'].includes(eventType);
                            if (targetEl && (!isMouseEvent || event.target === targetEl || targetEl.contains(event.target))) {
                                callback(event);
                            }
                        } else {
                            callback(event);
                        }
                    } catch (callbackErr) {
                        console.error('[harpe] addInformer callback error (id=' + id + '):', callbackErr);
                    }
                };

                if (!window.harpe_informers) { window.harpe_informers = {}; }

                // Remove existing listener if re-registering
                const isReReg = !!window.harpe_informers[id];
                if (isReReg) {
                    document.removeEventListener(window.harpe_informers[id].eventType, window.harpe_informers[id].handler);
                }

                window.harpe_informers[id] = { eventType, handler };
                console.log('[harpe] addInformer: ' + (isReReg ? 're-registering' : 'registering') + ' id=' + id + ' eventType=' + eventType + ' selector=' + selector);
                document.addEventListener(eventType, handler);
            } catch (err) {
                console.error('[harpe] addInformer setup failed (id=' + id + '):', err);
            }
        };

        window.removeInformer = (id) => {
            try {
                if (window.harpe_informers && window.harpe_informers[id]) {
                    const info = window.harpe_informers[id];
                    document.removeEventListener(info.eventType, info.handler);
                    delete window.harpe_informers[id];
                    console.log('[harpe] removeInformer: removed id=' + id);
                } else {
                    console.log('[harpe] removeInformer: nothing to remove for id=' + id);
                }
            } catch (err) {
                console.error('[harpe] removeInformer failed (id=' + id + '):', err);
            }
        };

        realInstance.exports.app_init();
    } catch (err) {
        console.error('[harpe] Failed to load WASM module:', err);
        if (err instanceof WebAssembly.CompileError) {
            console.error('[harpe] The .wasm binary may be corrupted or compiled with incompatible flags.');
        } else if (err instanceof TypeError) {
            console.error('[harpe] Check that ghc_wasm_jsffi.js is up-to-date and that all required exports exist in the WASM binary.');
        }
        console.error('[harpe] Stack:', err.stack);
    }
}

startWasm();

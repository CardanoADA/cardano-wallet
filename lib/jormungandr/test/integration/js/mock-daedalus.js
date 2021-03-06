#!/usr/bin/env node

// This runs cardano-wallet in the same way that Daedalus would.
// It needs node and cardano-wallet on the PATH to run.

const child_process = require("child_process");
const http = require('http');

function main() {
  const testName = process.argv[2];
  const cmd = process.argv[3];
  const args = process.argv.slice(4);

  const tests = {
    test1: function() {
      // test the different message types in sequence
      proc.on("message", function(msg) {
        console.log("JS: message received", msg);
        // See CardanoNode.js in Daedalus for the message types in use.
        if (msg.Started) {
          console.log("JS: sending a bogus message");
          proc.send("hello");
        } else if (msg.ParseError && msg.ParseError.match(/encountered String/)) {
          console.log("JS: sending QueryPort");
          proc.send({ QueryPort: [] });
        } else if (msg.ParseError) {
          console.log("JS: i did not expect that");
          process.exit(5);
        } else if (msg.ReplyPort) {
          let action = $onError => http.get({
              hostname: "localhost",
              port: msg.ReplyPort,
              path: "/v2/wallets",
              agent: false
            }, (res) => {
              console.log("JS: response from wallet: " + res.statusCode);
              res.resume();
              res.on("end", () => {
                console.log("JS: request response from wallet finished, disconnecting.");
                proc.disconnect();
              });
            }).on("error", $onError);

          let retryOnce = err => {
            if (err.code != 'ECONNREFUSED') {
              console.log("JS:", err);
              process.exit(1);
            } else {
              console.log("JS: Connection refused. Retrying once in a bit...");
              setTimeout(() => action(err => {
                console.log("JS:", err);
                process.exit(1);
              }), 500);
            }
          };

          action(retryOnce);
        }
      });
    },
    test2: function() {
      // regression test for #1036
      proc.send({ QueryPort: [] });
      proc.on("message", function(msg) {
        console.log("JS: message received", msg);
        if (msg.ReplyPort) {
          let action = $onError => http.get({
              hostname: "localhost",
              port: msg.ReplyPort,
              path: "/v2/wallets",
              agent: false
            }, (res) => {
              console.log("JS: response from wallet: " + res.statusCode);
              res.resume();
              res.on("end", () => {
                console.log("JS: request response from wallet finished, disconnecting.");
                proc.disconnect();
              });
            }).on("error", $onError);

          let retryOnce = err => {
            if (err.code != 'ECONNREFUSED') {
              console.log("JS:", err);
              process.exit(1);
            } else {
              console.log("JS: Connection refused. Retrying once in a bit...");
              setTimeout(() => action(err => {
                console.log("JS:", err);
                process.exit(1);
              }), 500);
            }
          };

          action(retryOnce);
        }
      });
    }
  };

  const test = tests[testName];

  if (!cmd || !test) {
    console.log("usage: node mock-daedalus.js (test1|test2) command [args]");
    process.exit(10);
  }

  const proc = child_process.spawn(cmd, args,
    { stdio: ["ignore", "inherit", "inherit", "ipc"] }
  );

  proc.on("close", function(code, signal) {
    console.log("JS: child_process stdio streams closed");
    process.exit(1);
  });

  proc.on("disconnect", function() {
    console.log("JS: child_process disconnected.");
    console.log("JS: waiting 2 seconds before exiting...");
    setTimeout(function() { process.exit(2); }, 2000);
  });

  proc.on("error", function(err) {
    console.log("JS: error child_process: " + err);
    process.exit(3);
  });

  proc.on("exit", function(code, signal) {
    console.log("JS: child_process exited with status " + code + " or signal " + signal);
    process.exit(code === 0 ? 0 : 4);
  });

  test();
}

main();

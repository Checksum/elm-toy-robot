import { Elm } from "./CLI.elm";
const readline = require("readline");

const cli = Elm.CLI.init();
const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: "> "
});

console.log("Elm Toy Robot. Enter a command: ");
rl.prompt();

cli.ports.put.subscribe(output => {
  console.log(output);
  rl.prompt();
});

rl.on("line", line => {
  cli.ports.get.send(line);
}).on("close", () => {
  console.log("Have a great day!");
  process.exit(0);
});

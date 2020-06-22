var jsonServer = require("json-server");
var server = jsonServer.create();
var router = jsonServer.router("db.json");
var middlewares = jsonServer.defaults();

server.use(middlewares);

server.use(jsonServer.bodyParser);

server.use((req, res, next) => {
  if (req.url === "/login" || req.url === "/heartbeat") {
    req.method = "GET";
    req.query = req.body;
  }
  next();
});

server.use(router);
server.listen(9001, (() => console.log("JSON Server is running")));

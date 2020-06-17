import "./main.css";
import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: { sessionId: localStorage.getItem("sessionId") },
});

app.ports.saveSessionId.subscribe((sessionId) => {
  if (sessionId === null) {
    localStorage.removeItem("sessionId");
  } else {
    localStorage.setItem("sessionId", sessionId);
  }
});

registerServiceWorker();

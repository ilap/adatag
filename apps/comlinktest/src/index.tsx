import { createRoot } from "react-dom/client";

import { WorkerContextProvider } from "./context/WorkerContextProvider";
import App from "./App";

const root = createRoot(document.getElementById("root")!)
root.render(
    <WorkerContextProvider>
      <App />
    </WorkerContextProvider>
);

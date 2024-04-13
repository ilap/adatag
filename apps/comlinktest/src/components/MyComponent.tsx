import React, { useContext } from "react";
import { WorkerStateContext } from "../context/WorkerContextProvider";

const MyComponent: React.FC = () => {
  const { syncState } = useContext(WorkerStateContext);

  return (
    <div>
      {/* Display the length */}
      {/* Display the status from the context */}
      <div>Status: {syncState} </div>
    </div>
  );
};

export default MyComponent;

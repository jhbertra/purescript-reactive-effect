export function newRaffController() {
  // We don't use the Performance API marks because the API is public and can
  // be tampered with by external code deleting them.
  const marks = { pausedAt: performance.now() };
  let totalPaused = marks.pausedAt;
  const getTime = () => {
    const totalTime = performance.now();
    return (isRunning() ? totalTime : marks.pausedAt) - totalPaused;
  };
  const isRunning = () => marks.hasOwnProperty("startedAt");
  const pause = () => {
    if (isRunning()) {
      delete marks.startedAt;
      marks.pausedAt = performance.now();
    }
  };
  const resume = () => {
    if (!isRunning()) {
      marks.startedAt = performance.now();
      totalPaused += marks.startedAt - marks.pausedAt;
      delete marks.pausedAt;
    }
  };
  return { getTime, isRunning, pause, resume };
}

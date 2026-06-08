export function _fetch(url) {
  return (onError, onSuccess) => {
    const controller = new AbortController();
    const signal = controller.signal;
    const cancel = () => controller.abort();
    signal.addEventListener("abort", () => {
      console.log("Fetch aborted");
    });
    fetch(url, { signal })
      .then((response) => {
        if (!response.ok) {
          onError(new Error(`HTTP error! status: ${response.status}`));
        } else {
          response.text().then(onSuccess).catch(onError);
        }
      })
      .catch(onError);
    return (cancelError, _onCancelerError, onCancelerSuccess) => {
      const aborted = cancel();
      console.log("aborted", aborted);
      onCancelerSuccess();
    };
  };
}

export function _get(url) {
  return (onError, onSuccess) => {
    const controller = new AbortController();
    const signal = controller.signal;
    const cancel = () => controller.abort();
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
      onCancelerSuccess();
    };
  };
}

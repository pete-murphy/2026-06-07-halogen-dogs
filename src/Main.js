export function _fetch(url) {
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

export function _setupRouting(onPathChange, onQueryChange) {
  const listener = (event) => {
    const oldURL = new URL(window.location);
    const newURL = new URL(event.destination.url);
    event.intercept({
      async handler() {
        if (oldURL.pathname != newURL.pathname) onPathChange(newURL.pathname);
        if (oldURL.searchParams.get("q") != newURL.searchParams.get("q"))
          onQueryChange(newURL.searchParams.get("q"));
      },
    });
  };

  navigation.addEventListener("navigate", listener);
  return () => navigation.removeEventListener("navigation", listener);
}

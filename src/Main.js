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

export function _setupRouting(onNavigate) {
  const listener = (event) => {
    const url = new URL(event.destination.url);
    event.intercept({
      async handler() {
        await onNavigate(url);
      },
    });
  };

  navigation.addEventListener("navigate", listener);
  return () => navigation.removeEventListener("navigation", listener);
}

export function pathname(url) {
  return url.pathname;
}
export function searchParams(url) {
  return url.searchParams;
}
export function eqURL(url0, url1) {
  return url0.pathname == url1.pathname && url0.search == url1.search;
}
export function currentURL() {
  return new URL(window.location);
}

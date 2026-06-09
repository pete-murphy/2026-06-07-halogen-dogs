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

export const baseURL = window.__BASE_URL__;

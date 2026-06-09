export function pathname(url) {
  return url.pathname;
}
export function searchParams(url) {
  return url.searchParams;
}
export function eqURL(url0, url1) {
  return url0.pathname == url1.pathname && url0.search == url1.search;
}
export function _fromLocation(location) {
  return new URL(location);
}
export function _eqURL(url0, url1) {
  return url0.pathname === url1.pathname && url0.search == url1.search;
}

import { defineConfig } from "vite";

// In production (GitHub Pages), the app is served from
// https://pete-murphy.github.io/2026-06-07-halogen-dogs/ so all asset URLs
// need that prefix. In dev (localhost), base stays "/".
export default defineConfig(({ command }) => ({
  base: command === "build" ? "/2026-06-07-halogen-dogs/" : "/",
  build: {
    outDir: "docs",
    emptyOutDir: true,
  },
}));

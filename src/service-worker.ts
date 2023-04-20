import { manifest, version } from "@parcel/runtime-service-worker";

async function install() {
  debugger;
  const cache = await caches.open(version);

  await cache.addAll(["/", "/index.html"]);
  await cache.addAll(manifest);
}
addEventListener("install", (e) => e.waitUntil(install()));

async function activate() {
  const keys = await caches.keys();
  await Promise.all(keys.map((key) => key !== version && caches.delete(key)));
}
addEventListener("activate", (e) => e.waitUntil(activate()));

async function putInCache(request: Request, response: Response) {
  const cache = await caches.open(version);
  await cache.put(request, response);
}

async function response(event: FetchEvent): Promise<Response> {
  var cachedResponse = await caches.match(event.request);
  if (cachedResponse) {
    return cachedResponse;
  }

  const response = await fetch(event.request);
  // Intentionall not awaited
  putInCache(event.request, response.clone());
  return response;
}

addEventListener("fetch", (e) => {
  e.respondWith(response(e));
});

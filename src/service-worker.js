self.addEventListener("install", function (e) {
  e.waitUntil(
    caches.open("progessive-elm").then(function (cache) {
      if (process.env.NODE_ENV !== "development") {
        return cache.addAll(['/', '/index.html']).then(
          cache.addAll(SERVICE_WORKER_MANIFEST_ENTRIES.map(e => e.url))
        );
      }
    })
  );
});

self.addEventListener("fetch", function (event) {
  event.respondWith(
    caches.match(event.request).then(function (response) {
      return response || fetch(event.request);
    })
  );
});

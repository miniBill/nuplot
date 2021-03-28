self.addEventListener("install", function (event) {
  event.waitUntil(
    caches.open("progressive-elm").then(function (cache) {
      return cache.addAll(['/', '/index.html']).then(
        cache.addAll(SERVICE_WORKER_MANIFEST_ENTRIES.map(entry => entry.url))
      );
    }).catch(function (ex) {
      debugger;
    })
  )
});

self.addEventListener("fetch", function (event) {
  event.respondWith(caches.open("progressive-elm").then((cache) =>
    fetch(event.request).then(function (response) {
      cache.put(event.request, response.clone());
    }).catch(function (ex) {
      debugger;
      return caches.match(event.request);
    }))
  )
});

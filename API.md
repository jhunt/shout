# The SHOUT! API

## Retrieving State

```
curl -X GET https://shout-ip/state
```

... which returns:

```
[
  {
    "name"  : "some-pipeline",
    "state" : "broken",

    "previous" : {
      "occurred-at" : 2208988500,
      "reported-at" : 2208988500,
      "ok"          : true,
      "message"     : "Pipeline build #361 succeeded",
      "link"        : "https://ci/p/some-pipeline/361"
    },
    "first" : {
      "occurred-at" : 2208988800,
      "reported-at" : 2208988800,
      "ok"          : false,
      "message"     : "Pipeline build #362 failed",
      "link"        : "https://ci/p/some-pipeline/362"
    },
    "last" : {
      "occurred-at" : 2208989999,
      "reported-at" : 2208989999,
      "ok"          : false,
      "message"     : "Pipeline build #366 failed",
      "link"        : "https://ci/p/some-pipeline/366"
    }
  }
]
```

You can also retrieve a single topic's state, by name:

```
curl -X GET https://shout-ip/state/some-pipeline
```

Which returns the same interior object as the collection endpoint.



## Posting Events

```
curl -X POST https://shout-ip/events -d '{
  "topic"       : "some-pipeline",
  "ok"          : true,
  "message"     : "Pipeline build #367 succeeded",
  "link"        : "https://ci/p/some-pipeline/367",
  "occurred-at" : 2208990000
}'
```

... which returns:

```
{
  "disposition": "recovered",
  "current": {
    "name"  : "some-pipeline",
    "state" : "working",

    "previous" : {
      "occurred-at" : 2208989999,
      "reported-at" : 2208989999,
      "ok"          : false,
      "message"     : "Pipeline build #366 failed",
      "link"        : "https://ci/p/some-pipeline/366"
    },
    "first" : {
      "occurred-at" : 2208990000,
      "reported-at" : 2208990004,
      "ok"          : true,
      "message"     : "Pipeline build #367 succeeded",
      "link"        : "https://ci/p/some-pipeline/367"
    },
    "last" : {
      "occurred-at" : 2208990000,
      "reported-at" : 2208990004,
      "ok"          : true,
      "message"     : "Pipeline build #367 succeeded",
      "link"        : "https://ci/p/some-pipeline/367"
    }
  }
}
```

This will cause SHOUT! to update its internal state database to
reflect the fact that _some-pipeline_ is now ok.  This may be a
continuation of a previous good state, in which case no
notification will go out, or it could be a recovery from a
previously bad state.

If _occurred-at_ is not given in the submitted payload, it will be
set to the current time.  The event's _reported-at_ is always set
server-side.

# The SHOUT! API

## Version and Release Information

```
curl https://shout-ip/info
```

... which returns:

```
{
  "version": "0.1.0",
  "release": "Whisper"
}
```

## Break/Fix Events

```
curl -u shout:shout -X POST https://shout-ip/events -d '{
  "topic"       : "some-pipeline",
  "ok"          : true,
  "message"     : "Pipeline build #367 succeeded",
  "link"        : "https://ci/p/some-pipeline/367",
  "occurred-at" : 2208990000,
  "metadata"    : {
    "optional"  : "metadata values",
    "that-get"  : "passed through untouched"
  }
}'
```

... which returns:

```
{
  "ok" : "Success!"
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

The _metadata_ key is optional, and provides user-definied (and
user-managed) key-value pairs that can be used to populate
messages, augment notification handling, or whatever you want.
Shout! neither interprets nor acts on your metadata of its own
accord.

## Announcements

```
curl -u shout:shout -X POST https://shout-ip/announcements -d '{
  "topic"   : "shout-releases",
  "message" : "Shout v1.2.3 (Mumble) Released!",
  "link"    : "https://github.com/jhunt/shout/releases/1.2.3"
}
```

... which returns:

```
{
  "ok" : "Success!"
}
```

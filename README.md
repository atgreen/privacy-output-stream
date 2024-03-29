# privacy-output-stream

`privacy-output-stream` is a privacy preserving character output
stream.  It wraps other streams, like `*standard-output*`, and masks
secrets from the output by replacing them with a series of `*`.

For example, the following code:
```
(let ((*standard-output* (make-instance 'privacy-output-stream
                                        :stream *standard-output*
                                        :secrets
                                        (mapcar #'secret-values:conceal-value
                                                '("passw0rd" "sekret")))))
  (format t "Hello, my password is passw0rd~%"))
```

...prints:
```
Hello, my password is ********
```

`secrets` is a list of concealed secret values you want to mask in
the final output.  Use `secret-values:conceal-value` to conceal them.
They are processed in order, so I recommend that you sort your secrets
by length (longest first) in case one is a substring of another.

Note also that `privacy-output-stream` only masks secrets that are
presented in full as part of a string or sequence being written to the
stream.  Secrets written in fragments (by character or substring) will
not be masked.

Author and License
-------------------

``privacy-output-stream`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.

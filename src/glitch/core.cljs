(ns glitch.core
  (:require [squelch.audio-context :as ac]
            [squelch.audio-node :as node]
            [squelch.audio-buffer :as ab]
            [squelch.audio-param :as param]
            [squelch.nodes.script-processor :as p]
            [squelch.nodes.wave-shaper :as ws]
            [squelch.nodes.biquad-filter :as bf]
            [squelch.nodes.delay :as dn]
            [squelch.nodes.audio-buffer-source :as bs]))

(def audio-ctx (ac/new-audio-context))

(def src-canvas (.getElementById js/document "src-canvas"))
(def src-ctx (.getContext src-canvas "2d"))

(def dest-canvas (.getElementById js/document "dest-canvas"))
(def dest-ctx (.getContext dest-canvas "2d"))

(def processor-buffer-size 256)

(defn size-canvas
  [canvas width height]
  (set! (.-width canvas) width)
  (set! (.-height canvas) height)
  (set! (.-width (.-style canvas)) width)
  (set! (.-height (.-style canvas)) height))

(defn scaled-height
  "Takes a base set of dimensions and returns a scaled set based on the max
  width"
  [base-dimensions max-width]
  (let [base-width (base-dimensions 0)
        base-height (base-dimensions 1)]
        (* max-width (/ base-height base-width))))

(defn set-up-canvases
  [image]
  (let [img-width image.width
        img-height image.height
        max-width (.-clientWidth src-canvas)
        max-height (scaled-height [img-width img-height] max-width)]
    (doall (map #(size-canvas %1 max-width max-height)
                [src-canvas dest-canvas]))
    (set! src-ctx (.getContext src-canvas "2d"))
    (set! dest-ctx (.getContext dest-canvas "2d"))
    (.drawImage src-ctx image 0 0 max-width max-height)))

(defn uint8-to-float32
  [uint8]
  (- (* 2 (/ uint8 255)) 1))

(defn float32-to-uint8
  [float32]
  (Math/floor (* 255 (/ (+ float32 1) 2))))

(defn rgba-to-4-channel-audio
  [uint8-array]
  (let [r (take-nth 4 (array-seq uint8-array))
        g (take-nth 4 (drop 1 (array-seq uint8-array)))
        b (take-nth 4 (drop 2 (array-seq uint8-array)))
        a (take-nth 4 (drop 3 (array-seq uint8-array)))
        float-r (js/Float32Array. (to-array (map uint8-to-float32 r)))
        float-g (js/Float32Array. (to-array (map uint8-to-float32 g)))
        float-b (js/Float32Array. (to-array (map uint8-to-float32 b)))
        float-a (js/Float32Array. (to-array (map uint8-to-float32 a)))]
    [float-r float-g float-b float-a]))

(defn copy-channels-to-buffer
  [channels buffer]
  (doall (map-indexed (fn
                        [idx channel]
                        (let [dest-channel (ab/get-channel-data buffer idx)]
                          (.set dest-channel channel)))
                        channels)))

(defn render-audio-fn
  [width height]
  (let [pointer (atom {:x 0 :y 0})
        image-data (.getImageData dest-ctx 0 0 width height)]
    (fn
      [input-buffer output-buffer]

      (let [float-r (ab/get-channel-data input-buffer 0)
            float-g (ab/get-channel-data input-buffer 1)
            float-b (ab/get-channel-data input-buffer 2)
            float-a (ab/get-channel-data input-buffer 3)
            out-r (ab/get-channel-data output-buffer 0)
            out-g (ab/get-channel-data output-buffer 1)
            out-b (ab/get-channel-data output-buffer 2)
            out-a (ab/get-channel-data output-buffer 3)
            loop-range (range 0 processor-buffer-size)]

        (.set out-r float-r)
        (.set out-g float-g)
        (.set out-b float-b)
        (.set out-a float-a)

        (doall (map #(let [x (:x @pointer)
                           y (:y @pointer)
                           x-overruns? (>= (inc x) width)
                           next-x (if x-overruns? 0 (inc x))
                           next-y (if x-overruns? (inc y) y)
                           r (float32-to-uint8 (aget float-r %))
                           g (float32-to-uint8 (aget float-g %))
                           b (float32-to-uint8 (aget float-b %))
                           a (float32-to-uint8 (aget float-a %))
                           pos (* (+ x (* y width)) 4)]

                       (if-not (> pos (.-length (.-data image-data)))
                         ((swap! pointer assoc :x next-x)
                          (swap! pointer assoc :y next-y)
                          (.set (.-data image-data) (array r g b a) pos)))

                      ) loop-range))
        (.putImageData dest-ctx image-data 0 0)
        ))))

; From here: http://kevincennis.github.io/transfergraph/ and
; http://stackoverflow.com/questions/22312841/waveshaper-node-in-webaudio-how-to-emulate-distortion
(defn make-distortion-curve
  [amount]
  (let [num-samples 44100
        curve (js/Float32Array. num-samples)
        deg (/ Math/PI 180)]

    (doall (map #(let [x (- (/ (* % 2) num-samples) 1)
                       value (/ (* (+ 3 amount) x 20 deg)
                                (+ Math/PI (* amount (Math/abs x))))]
                   (aset curve % value)
                   ) (range 0 num-samples)))
    curve))

(defn start-processing
  [image]
  (fn []
    (set-up-canvases image)

    (let [width (.-width src-canvas)
          height (.-height src-canvas)
          image-data (.getImageData src-ctx 0 0 width height)]

      (let [channels (rgba-to-4-channel-audio (.-data image-data))
            sample-rate (ac/get-sample-rate audio-ctx)
            length (.-length (channels 0))
            num-channels (count channels)
            buffer (ac/create-buffer audio-ctx num-channels length sample-rate)
            buffer-source (ac/create-buffer-source audio-ctx)
            processor (ac/create-script-processor audio-ctx
                                                  processor-buffer-size
                                                  num-channels
                                                  num-channels)
            process-func (render-audio-fn width height)
            distortion (ac/create-wave-shaper audio-ctx)
            distortion-curve (make-distortion-curve -2)
            high-pass (ac/create-biquad-filter audio-ctx)
            low-pass (ac/create-biquad-filter audio-ctx)
            high-shelf (ac/create-biquad-filter audio-ctx)
            low-shelf (ac/create-biquad-filter audio-ctx)
            splitter (ac/create-channel-splitter audio-ctx 4)
            merger (ac/create-channel-merger audio-ctx 4)
            r-delay (ac/create-delay audio-ctx 10)
            destination (ac/get-destination audio-ctx)]

        ; If the processor isn't stored in the global scope, it gets GCed. This
        ; is only a bug on Chrome. Garbage collecting source maps seems to be a
        ; consistent problem in chrome.
        (set! (.-processor js/window) processor)

        ; Build the buffer
        (copy-channels-to-buffer channels buffer)
        (bs/set-buffer buffer-source buffer)

        ; Start chaining things together
        ;(node/connect buffer-source processor)

        ;(node/connect buffer-source distortion)
        ;(node/connect distortion processor)

        ;(node/connect buffer-source high-pass)
        ;(node/connect high-pass processor)

        ;(node/connect buffer-source low-pass)
        ;(node/connect low-pass high-pass)
        ;(node/connect high-pass processor)

        ;(node/connect buffer-source low-shelf)
        ;(node/connect low-shelf high-shelf)
        ;(node/connect high-shelf processor)

        ;(node/connect buffer-source low-shelf)
        ;(node/connect low-shelf low-pass)
        ;(node/connect low-pass high-shelf)
        ;(node/connect high-shelf high-pass)
        ;(node/connect high-pass processor)

        (node/connect buffer-source splitter)
        (node/connect splitter r-delay 0)
        (node/connect r-delay merger 0 0)
        ;(node/connect splitter high-shelf 1)
        ;(node/connect high-shelf low-shelf)
        ;(node/connect low-shelf merger 0 1)
        (node/connect splitter merger 1 1)
        (node/connect splitter merger 2 2)
        (node/connect splitter merger 3 3)
        (node/connect merger processor)
        (node/connect processor destination)

        ; Set up the delay
        (param/set-value (dn/get-delay-time r-delay) 0.5)

        ; Set up the high pass
        (bf/set-type high-pass "highpass")
        (param/set-value (bf/get-Q high-pass) 1)
        (param/set-value (bf/get-frequency high-pass) 100)

        ; Set up the high pass
        (bf/set-type low-pass "lowpass")
        (param/set-value (bf/get-Q low-pass) 1)
        (param/set-value (bf/get-frequency low-pass) 1000)

        ; Set up the high shelf
        (bf/set-type high-shelf "highshelf")
        (param/set-value (bf/get-gain high-shelf) 6)
        (param/set-value (bf/get-frequency high-shelf) 5000)

        ; Set up the high shelf
        (bf/set-type low-shelf "lowshelf")
        (param/set-value (bf/get-gain low-shelf) 6)
        (param/set-value (bf/get-frequency low-shelf) 100)

        ; Set up the distortion
        (ws/set-curve distortion distortion-curve)

        ; This is our link from the audio bus back into js space
        (p/set-on-audio-process processor process-func)

        ; Start
        (bs/start buffer-source)
        ))))

(def src-img (js/Image.))
(set! (.-src src-img) "/mina.jpg")
(set! (.-onload src-img) (start-processing src-img))

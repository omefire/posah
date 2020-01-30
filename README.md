# posah

In order to run this app, you'll need to:

    - Install secp256k1 on the host machine:
        - git clone git@github.com:bitcoin-core/secp256k1.git (tested with commit d644dda5c9dbdecee52d1aa259235510fdc2d4ee)
        - cd secp256k1
        - ./autogen.sh
        - ./configure
        - make
        - make check
        - sudo make install  # optional

    - Build the project: 
        - stack build

    - Run the project:
        - stack run

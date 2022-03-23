long xorEncrypt() {
    unsigned char *dataBuffer = * (unsigned char **) 0x10;
    long dataBufferLen = * (long *) 0x18;
    unsigned char *keyBuffer = * (unsigned char **) 0x20;
    long keyBufferLen = * (long *) 0x28;
    long i;
    long keyIndex = 0;
    for (i = 0; i < dataBufferLen; i++) {
        dataBuffer[i] = dataBuffer[i] ^ keyBuffer[keyIndex++];
        if(keyIndex == keyBufferLen) keyIndex = 0;
    }
    return 0;
}
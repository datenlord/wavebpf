long xorEncrypt(unsigned char *dataBuffer, long dataBufferLen, unsigned char *keyBuffer, long keyBufferLen) {
    int i;
    int keyIndex = 0;
    for (i = 0; i < dataBufferLen; i++) {
        dataBuffer[i] = dataBuffer[i] ^ keyBuffer[keyIndex++];
        if(keyIndex == keyBufferLen) keyIndex = 0;
    }
    return 0;
}
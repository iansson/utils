import collections
import re
from PIL import Image
import numpy as np
import sys


def str_to_bin(str : str) -> str:
    # Different approaches to achieve the same thing: 
    # bits = [format(ord(char), '08b') for char in msg]                 # eg. ['01010111', '01100001', '01110010', '00100000']
    # from https://www.geeksforgeeks.org/python/python-convert-string-to-binary/ :
    # bits = ''.join(format(ord(char), '08b') for char in msg)          # eg. "01010111011000010111001000100000011000010110111001"
    bits = [bit for char in str for bit in format(ord(char), '08b')]    # eg. ['0', '1', '0', '1', '0', '1', '1', '1', '0']
    return bits

def bin_to_str(bits : list) -> str:
    # for every 8 elements in bits, join 8 bits to a "bitstring" and convert them to a unicode character
    chars = [chr(int(''.join(bits[i:i+8]), 2)) for i in range(0, len(bits), 8)]
    original_msg = ''.join(chars)
    return original_msg


def load_image(path : str):
    img = Image.open(path) 
    pixels = np.array(img, dtype=np.uint8) # this gives us a 2D array, pixels[0][0] gives us the data of the first pixel
    # example pixel: [224 238 241]
    return pixels

def store_image(pixels, file_name : str):
    # side effects: saves image

    image = Image.fromarray(pixels, 'RGB')
    image.save(file_name)


def encode_on_pixel(pixel, msg_bit: str):
    # side effects: (none)
    
    if len(msg_bit) != 1:
        raise ValueError(f"Error: Invalid bit length: {len(msg_bit)}")
    
    if msg_bit != '1' and msg_bit != '0':
        raise ValueError(f"Error: Invalid message bit: {msg_bit}")
    
    red_channel, green_channel, blue_channel = pixel

    binary = list(f'{blue_channel:08b}') # Assuming the channel consists of 8 bits (which is standard in most formats)
    binary[-1] = msg_bit

    new_pixel = red_channel, green_channel, np.uint8(int(''.join(binary), 2))

    return new_pixel

def encode_message(pixels, msg):
    # WARNING FOR SIDE EFFECTS: 'pixels' will be altered direcly
    
    msg_with_len = str(len(msg)) + "||" + msg
    encoded_msg = str_to_bin(msg_with_len)

    bit_index = 0

    for i, row in enumerate(pixels):
        for j, pixel in enumerate(row):
            bit = encoded_msg[bit_index]
            # print(f"before: {pixel}")
            pixels[i][j] = encode_on_pixel(pixel, bit)
            # print(f"after: {pixel}")
            bit_index += 1
            if bit_index >= len(encoded_msg): break
        if bit_index >= len(encoded_msg): break

    return pixels

def decode_pixel(pixel):
    red_channel, green_channel, blue_channel = pixel
    binary = list(f'{blue_channel:08b}') # Assuming the channel consists of 8 bits (which is standard in most formats)
    return binary[-1]

def decode_message(pixels):
    bits = []

    for row in pixels:
        for pixel in row:
            bits.append(decode_pixel(pixel))

    decoded = bin_to_str(bits)

    msg_size_str, msg = re.split(r"\|\|", decoded, maxsplit=1)
    msg_size = int(msg_size_str)
    
    return msg[:msg_size]

def main():
    # if len(sys.argv) < 2:
    #     print("Usage: python steganography.py <image_path>")
    #     return
    # image_path = sys.argv[1]
    # pixels = load_image(image_path)

    msg = "War and hate, war and hate!"

    pixels1 = load_image("nomorefanmail.png")
    encode_message(pixels1, msg)
    store_image(pixels1, "secret.png") ## You can't store in jpg, since it's a lossy format. The message will break!

    decoded1 = decode_message(pixels1)

    print(f"secret message:           {msg}")
    print(f"decoded from image:       {decoded1}")

    pixels2 = load_image("secret.png") 
    decoded2 = decode_message(pixels2)
    print(f"decoded from saved image: {decoded2}")

if __name__ == "__main__":
        main()
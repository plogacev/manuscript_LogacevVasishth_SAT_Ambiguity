import array, math, numpy, struct, ctypes
import pygame
import time

sampling_rate=44100
size=16
pygame.mixer.quit()
pygame.mixer.init()#sampling_rate, size=-size, channels=1)

channel = pygame.mixer.Channel(7)
channel.set_volume(1.0)

class Sine:

    def __init__(self, frequency, duration):
		duration_in_samples = sampling_rate*duration
		samples = [5000*math.sin(2.0 * math.pi * frequency * t / sampling_rate) for t in xrange(0, duration_in_samples)]
		
		samples = numpy.array(samples, dtype=numpy.int16)
		self.sound = pygame.sndarray.make_sound(samples)
		self.sound.set_volume(1.0)
		
	
    def play(self):
		channel.play(self.sound)
		
		while pygame.mixer.get_busy():
			pygame.time.wait(200)
	
#    def __del__(self):	
#	pygame.mixer.quit()

from .Experiment import Experiment

class Interact(Experiment):

    def __init__(self, run_config):
        super().__init__()
        self.name = 'Interact'

        #Load model
        self.get_models(run_config)

        if ('include_punct' not in run_config or 
                not run_config['include_punct']):
            self.include_punctuation = False
        else:
            self.include_punctuation = True

    def run_interact(self):

        header = ['word', 'isSplit', 'isUnk', 'withPunct', 'modelName', 'surp', 'prob']
        header = "{:>10}   {:>6} {:>6} {:>10} {:>40} {:>6} {:>8}".format(*header)

        while True:
            sent = input('string: ').strip()
            print(header)
            #print('\t'.join(header))
            output = self.get_interactive_output(sent, self.include_punctuation)
            for word in output:
                print_out = [word.word, word.isSplit, word.isUnk, word.withPunct, 
                        word.modelName.split('/')[-1], 
                        round(word.surp, 3), round(2**(-word.surp), 5)]
                print_out = "{:>10}   {:>6} {:>6} {:>10} {:>40} {:>6} {:>8}".format(*print_out)

                print(print_out)
            print()

    def get_interactive_output(self, sent, include_punctuation=False):

        assert len(self.model_instances) == 1, 'No models loaded for interactive testing'

        for model in self.model_instances:
            #Get output and flatten
            output_by_word = model.get_aligned_words_surprisals(
                                sent, include_punctuation)[0]
            return output_by_word

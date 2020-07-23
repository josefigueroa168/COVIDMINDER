import  pandas as pd
import numpy as np
import warnings
from statsmodels.tsa.statespace.sarimax import SARIMAX

DEBUG = True

class Predictor():
    def __init__(self,path,col,state):
        self.path=path+'/data/csv/time_series/covid_TS_counties_long.cases.csv'
        self.col=col
        self.state=state
    def Build_Data(self):
        self.Data=pd.read_csv(self.path,parse_dates=True)
        self.Data=self.Data[self.Data['State']==self.state]
        self.Counties=pd.unique(self.Data['County'])

        self.Data_Dates=self.Data.pivot_table(values=self.col,
            index='date',columns='County',aggfunc='first')

        self.Data_Dates.fillna(value=0,inplace=True)
        return(self.Data_Dates)
    def Build_Training_Data(self,v=7,training=True):
        if training :
            self.v=v
        nan_arr = np.empty((5,self.Data_Dates.shape[1]))
        nan_arr[:] = np.nan

        if DEBUG:
            print("Tail: ", self.Data_Dates.tail(v).to_numpy().shape)
            print("NaN: ", nan_arr.shape)
        self.Values=np.append(self.Data_Dates.head(-v).to_numpy(),nan_arr,axis=0).flatten('F').tolist()
        if DEBUG:
            display(self.Values)
    def GridSearch(self,n_days):
        self.Build_Training_Data(v=n_days,training=True)
        warnings.filterwarnings("ignore")
        params=[]
        scores=[]
        for p in range(1, 5):
            for q in range(1, 5):
                for d in range(3):
                    if DEBUG:
                        print("Running models with (p,d,q) as: ", (p,d,q))
                    try :
                        model = SARIMAX(self.Values, order=(p, d, q),missing='drop', enforce_invertibility=False)
                        results = model.fit(disp=0)
                        ## Jose vectorized code
                        DataCounty = self.Data_Dates
                        ModelCounty = DataCounty.head(-self.v).apply(lambda x: 
                            SARIMAX(x, order=(p, d, q), missing = 'drop', enforce_invertibility=False))
                        res = ModelCounty.apply(lambda x: x.smooth(results.params))
                        fc = res.apply(lambda x: 
                            x.get_prediction(DataCounty.shape[0] - self.v, DataCounty.shape[0]))
                        frame = fc.apply(lambda x: x.summary_frame(alpha = 0.5))
                        fc = frame.apply(lambda x: x['mean'])
                        Y = DataCounty.tail(self.v).to_numpy().T
                        Yhat = fc.T.tail(self.v).to_numpy().T
                        MAE = np.sum(abs(Y - Yhat),axis=1)/self.v

                        ## Original
                        #for county in self.Counties:
                        #    DataCounty = self.Data_Dates[county].dropna()
                        #    ModelCounty = SARIMAX(DataCounty.head(-self.v), order=(p, d, q), missing='drop',
                        #                          enforce_invertibility=False)
                        #    res = ModelCounty.smooth(results.params)
                        #    fc = res.get_prediction(len(DataCounty) - self.v, len(DataCounty))
                        #    frame = fc.summary_frame(alpha=0.05)
                        #    fc = frame['mean']
                        #    Y = DataCounty.iloc[-self.v:].values
                        #    #display(Y)
                        #    Yhat = fc[-self.v:].values

                        #    print(Y.shape, Yhat.shape)
                            # Ybar = np.mean(Y)
                        #    MAE = (sum(abs(Y - Yhat)) / self.v)
                        #    print("MAE: ", MAE)
                        #    scores_counties.append(MAE)
                    except :
                        print('Training failed for parameters :',(p,d,q))
                    scores.append(np.nanmean(MAE))
                    params.append((p, d, q))
                    if DEBUG:
                        print("Score: ", scores[-1])
        argbest = np.argmin(scores)
        print('Best MAE : ', scores[argbest])
        print('Best params : ', params[argbest])
        self.BestParams = params[argbest]
    def Prediction(self,n_days,params='Best'):
        if params=='Best':
            try :
                params=self.BestParams
            except :
                print('Please do a grid search to find the best parameters')
        self.Build_Training_Data(v=0,training=False)
        BestMod = SARIMAX(self.Values, order=params, missing='drop',
                          enforce_invertibility=False)
        BestRes = BestMod.fit(disp=0)
        self.Pred= pd.DataFrame(columns=['County', 'mean', 'mean_ci_upper', 'mean_ci_lower'])

        # Jose Testing vectorized output
        #DataCounty = self.Data_Dates
        #ModelCounty = DataCounty.apply(lambda x: 
        #    SARIMAX(x, order=params, missing = 'drop', enforce_invertibility=False))
        #res = ModelCounty.apply(lambda x: x.smooth(BestRes.params))
        #fc = res.apply(lambda x: 
        #    x.get_prediction(0, DataCounty.shape[0] + n_days))
        #frame = fc.apply(lambda x: x.summary_frame(alpha = 0.5))
        #fc = frame.apply(lambda x: x['mean'])
        #confInf = frame.apply(lambda x: x['mean_ci_lower'])
        #confSup = frame.apply(lambda x: x['mean_ci_upper'])
        #display(frame[['County', 'mean', 'mean_ci_upper', 'mean_ci_lower']])

        # Original Code
        for county in self.Counties:
            DataCounty=self.Data_Dates[county]
            ModelCounty = SARIMAX(DataCounty, order=params, missing='drop', enforce_invertibility=False)
            res = ModelCounty.smooth(BestRes.params)
            fc = res.get_prediction(0, len(DataCounty) + n_days)
            frame = fc.summary_frame(alpha=0.05)
            fc = frame['mean']
            confInf = frame['mean_ci_lower']
            confSup = frame['mean_ci_upper']
            frame['County']=[county]*len(frame)
            self.Pred=self.Pred.append(frame[['County', 'mean', 'mean_ci_upper', 'mean_ci_lower']])
        self.Pred.index.name = 'date'
        return(self.Pred)
    def save_pred(self,pathsave):
        self.Pred.to_csv(pathsave)

if __name__=="__main__":
    path = 'C:/Users/louis/OneDrive/Documents/GitHub/COVIDMINDER'
    pred=Predictor(path,col='p_diff',state='AK')
    pred.Build_Data()
    pred.GridSearch(n_days=7)
    prediction=pred.Prediction(n_days=7)
    #pred.save_pred(path+'/Predictions_NY.csv')



